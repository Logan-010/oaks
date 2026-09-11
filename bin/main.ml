let mode = Cli.parse ()

let send_all sock value =
  let bytes = Bytes.of_string value in
  let rec loop offset =
    if offset < Bytes.length bytes then
      let sent = Unix.write sock bytes offset (Bytes.length bytes - offset) in
      if sent = 0 then failwith "socket closed while sending"
      else loop (offset + sent)
  in
  loop 0

let hex_digit value =
  if value < 10 then Char.chr (Char.code '0' + value)
  else Char.chr (Char.code 'a' + value - 10)

let hex_of_bytes bytes =
  String.init
    (Bytes.length bytes * 2)
    (fun index ->
      let byte = Char.code (Bytes.get bytes (index / 2)) in
      if index mod 2 = 0 then hex_digit (byte lsr 4)
      else hex_digit (byte land 0xf))

let byte_of_hex_digit = function
  | '0' .. '9' as digit -> Char.code digit - Char.code '0'
  | 'a' .. 'f' as digit -> Char.code digit - Char.code 'a' + 10
  | 'A' .. 'F' as digit -> Char.code digit - Char.code 'A' + 10
  | _ -> invalid_arg "Invalid hexadecimal secret"

let bytes_of_hex value =
  if String.length value mod 2 <> 0 then
    invalid_arg "Invalid hexadecimal secret"
  else
    Bytes.init
      (String.length value / 2)
      (fun index ->
        let high = byte_of_hex_digit value.[index * 2] in
        let low = byte_of_hex_digit value.[(index * 2) + 1] in
        Char.chr ((high lsl 4) lor low))

let () =
  match mode with
  | Cli.Help ->
      prerr_string Cli.help;
      exit 0
  | _ ->
      let net = Net.create 0 in
      Unix.set_nonblock net;
      let client = Oaks.create @@ Net.used_port net in
      let discovery_task = Oaks.start client in

      let stop = Atomic.make false in
      let is_done = Atomic.make false in

      ignore
      @@ Sys.signal Sys.sigint
           (Sys.Signal_handle (fun _ -> Atomic.set stop true));

      let task =
        Domain.spawn (fun () ->
            Fun.protect
              ~finally:(fun () -> Atomic.set is_done true)
              (fun () ->
                match mode with
                | Cli.Send path ->
                    Printf.eprintf "%s\n%!" (Oaks.secret client |> hex_of_bytes);

                    let rec accept_loop listen_fd =
                      if Atomic.get stop then ()
                      else
                        let readable, _, _ =
                          try Unix.select [ listen_fd ] [] [] 0.25
                          with Unix.Unix_error (Unix.EINTR, _, _) ->
                            ([], [], [])
                        in
                        if readable = [] then accept_loop listen_fd
                        else
                          let client_fd, addr = Unix.accept listen_fd in

                          ignore
                            (Domain.spawn (fun () ->
                                 Fun.protect
                                   ~finally:(fun () -> Unix.close client_fd)
                                   (fun () ->
                                     match addr with
                                     | Unix.ADDR_INET (ip, port) ->
                                         let input = open_in path in
                                         let name =
                                           String.split_on_char '/' path
                                           |> List.rev |> List.hd
                                         in

                                         Fun.protect
                                           ~finally:(fun () -> close_in input)
                                           (fun () ->
                                             Printf.printf
                                               "connection from %s:%d\n%!"
                                               (Unix.string_of_inet_addr ip)
                                               port;

                                             let key =
                                               Oaks.auth client_fd
                                                 (Oaks.secret client)
                                             in
                                             Printf.printf "%s\n%!"
                                               (hex_of_bytes key);

                                             send_all client_fd name;

                                             let len = in_channel_length input in
                                             seek_in input 0;
                                             let data = really_input_string input len in
                                             send_all client_fd data)
                                     | Unix.ADDR_UNIX _ -> ())));

                          accept_loop listen_fd
                    in

                    accept_loop net;

                    ()
                | Cli.Recv (raw_secret, path) ->
                    let secret = bytes_of_hex raw_secret in

                    let peer = Oaks.discover client secret in
                    let peer_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

                    Fun.protect
                      ~finally:(fun () -> Unix.close peer_fd)
                      (fun () ->
                        Unix.connect peer_fd peer;
                        let key = Oaks.authed peer_fd secret in
                        Printf.printf "%s\n%!" (hex_of_bytes key));
                    ignore path
                | Cli.Help -> ()))
      in

      while (not (Atomic.get stop)) && not (Atomic.get is_done) do
        Unix.sleepf 0.05
      done;

      Atomic.set stop true;
      Domain.join task;
      ignore (Oaks.stop client discovery_task)
