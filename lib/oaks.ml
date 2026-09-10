type mdns_client = {
  sock : Unix.file_descr;
  bytes : Bytes.t;
  stop : bool Atomic.t;
}

let mdns_addr = Unix.inet_addr_of_string "224.0.0.251"

let spawn_socket port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in

  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt sock Unix.SO_REUSEPORT true;

  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));

  Lwt_unix.mcast_add_membership
    (Lwt_unix.of_unix_file_descr ~blocking:true sock)
    mdns_addr;

  sock

let default_client () =
  {
    sock = spawn_socket 5353;
    bytes = Bytes.create 4096;
    stop = Atomic.make false;
  }

let discovery_loop client =
  let buflen = Bytes.length client.bytes in

  while not (Atomic.get client.stop) do
    let readable, _, _ = Unix.select [ client.sock ] [] [] 0.25 in

    if readable <> [] then
      let len, _sender = Unix.recvfrom client.sock client.bytes 0 buflen [] in

      match
        Dns.Packet.decode (String.of_bytes (Bytes.sub client.bytes 0 len))
      with
      | Ok _p -> print_endline "got packet!"
      | Error e -> Fmt.epr "%a@." Dns.Packet.pp_err e
  done

let start client = Domain.spawn (fun _ -> discovery_loop client)

let stop client task =
  Atomic.set client.stop true;
  Domain.join task
