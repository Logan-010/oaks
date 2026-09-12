module StringMap = Map.Make (String)

type mdns_client = {
  sock : Unix.file_descr;
  bytes : Bytes.t;
  stop : bool Atomic.t;
  port : int;
  secret : Bytes.t;
  mutable addrs : Unix.sockaddr StringMap.t;
  mutex : Mutex.t;
}

let mdns_addr = Unix.inet_addr_of_string "224.0.0.251"

let spawn_socket port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in

  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt sock Unix.SO_REUSEPORT true;

  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));

  let lwt_sock = Lwt_unix.of_unix_file_descr ~blocking:true sock in

  Lwt_unix.mcast_add_membership lwt_sock mdns_addr;
  Lwt_unix.mcast_set_loop lwt_sock true;

  sock

let create port =
  Mirage_crypto_rng_unix.use_default ();

  {
    sock = spawn_socket 5353;
    bytes = Bytes.create 4096;
    stop = Atomic.make false;
    port;
    secret = Mirage_crypto_rng_unix.getrandom 32 |> Bytes.of_string;
    addrs = StringMap.empty;
    mutex = Mutex.create ();
  }

let id client =
  client.secret |> Digestif.BLAKE2B.digest_bytes |> Digestif.BLAKE2B.to_hex

let discovery_loop client =
  let buflen = Bytes.length client.bytes in

  let id = id client in

  let peer_name = Domain_name.of_string_exn "oaks._oaks._udp.local" in

  let question = Dns.Packet.Question.create peer_name Dns.Rr_map.Txt in

  let txt_records =
    Dns.Rr_map.Txt_set.of_list
      [ Fmt.str "id=%s" id; Fmt.str "port=%d" client.port ]
  in
  let additional =
    Dns.Name_rr_map.singleton peer_name Dns.Rr_map.Txt (60l, txt_records)
  in
  let packet =
    Dns.Packet.create ~additional (0, Dns.Packet.Flags.empty) question `Query
  in

  let encoded, _ = Dns.Packet.encode `Udp packet in
  let encoded = Bytes.unsafe_of_string encoded in

  let mdns_target = Unix.ADDR_INET (mdns_addr, 5353) in

  let send_beacon () =
    ignore
      (Unix.sendto client.sock encoded 0 (Bytes.length encoded) [] mdns_target)
  in

  let last_send = ref neg_infinity in
  let interval = 3.0 in

  while not (Atomic.get client.stop) do
    let now = Unix.gettimeofday () in

    if now -. !last_send >= interval then begin
      send_beacon ();
      last_send := now
    end;

    let readable, _, _ = Unix.select [ client.sock ] [] [] 0.25 in

    if readable <> [] then begin
      let len, sender = Unix.recvfrom client.sock client.bytes 0 buflen [] in

      let data = Bytes.sub_string client.bytes 0 len in

      match Dns.Packet.decode data with
      | Error e -> Fmt.epr "%a@." Dns.Packet.pp_err e
      | Ok packet ->
          let name, _qtype = packet.question in
          let name = Domain_name.to_string name in

          if name = Domain_name.to_string peer_name then
            match
              Dns.Name_rr_map.find peer_name Dns.Rr_map.Txt packet.additional
            with
            | Some (_ttl, records) ->
                let records = Dns.Rr_map.Txt_set.elements records in
                let value prefix =
                  List.find_map
                    (fun record ->
                      if String.starts_with ~prefix record then
                        Some
                          (String.sub record (String.length prefix)
                             (String.length record - String.length prefix))
                      else None)
                    records
                in
                begin match (value "id=", value "port=", sender) with
                | Some peer_id, Some port_str, Unix.ADDR_INET (addr, _mdns_port)
                  when peer_id <> id ->
                    begin match int_of_string_opt port_str with
                    | Some peer_port when peer_port >= 0 && peer_port <= 65535
                      ->
                        let peer_addr = Unix.ADDR_INET (addr, peer_port) in

                        Fmt.pr "discovered peer %s at %s:%d@." peer_id
                          (Unix.string_of_inet_addr addr)
                          peer_port;

                        Mutex.protect client.mutex @@ fun _ ->
                        client.addrs <-
                          StringMap.add peer_id peer_addr client.addrs
                    | _ -> ()
                    end
                | _ -> ()
                end
            | None -> ()
          else ()
    end
  done

let start client = Domain.spawn (fun _ -> discovery_loop client)

let stop client task =
  Atomic.set client.stop true;
  Domain.join task

let secret client = client.secret

let auth_key nonce secret =
  Bytes.cat nonce secret |> Bytes.to_string |> Digest.BLAKE256.string
  |> Bytes.of_string

let discover client secret =
  let id = secret |> Digestif.BLAKE2B.digest_bytes |> Digestif.BLAKE2B.to_hex in

  let rec loop () =
    try Mutex.protect client.mutex @@ fun _ -> StringMap.find id client.addrs
    with Not_found -> loop ()
  in

  loop ()

let send sock bytes =
  let rec loop offset =
    if offset < Bytes.length bytes then begin
      let sent = Unix.write sock bytes offset (Bytes.length bytes - offset) in
      if sent = 0 then failwith "socket closed while sending";
      loop (offset + sent)
    end
  in
  loop 0

let recv sock len =
  let buf = Bytes.create len in

  let rec loop offset =
    if offset = len then buf
    else
      let n = Unix.read sock buf offset (len - offset) in
      if n = 0 then failwith "socket closed before receiving expected data"
      else loop (offset + n)
  in

  loop 0

let auth sock secret =
  let nonce = Mirage_crypto_rng_unix.getrandom 32 |> Bytes.of_string in

  send sock nonce;

  auth_key nonce secret

let authed sock secret =
  let nonce = recv sock 32 in
  auth_key nonce secret
