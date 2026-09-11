let mode = Cli.parse ()

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
                    Printf.eprintf "%s\n%!"
                      (Oaks.secret client |> Hex.hex_of_bytes);

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
                                         Printf.printf
                                           "connection from %s:%d\n%!"
                                           (Unix.string_of_inet_addr ip)
                                           port;

                                         let input = open_in path in
                                         let name =
                                           String.split_on_char '/' path
                                           |> List.rev |> List.hd
                                         in

                                         ignore name;

                                         Fun.protect
                                           ~finally:(fun () -> close_in input)
                                           (fun () ->
                                             let key =
                                               Oaks.auth client_fd
                                                 (Oaks.secret client)
                                             in

                                             File.send input client_fd key)
                                     | Unix.ADDR_UNIX _ -> ())));

                          accept_loop listen_fd
                    in

                    accept_loop net;

                    ()
                | Cli.Recv (raw_secret, path) ->
                    let secret = Hex.bytes_of_hex raw_secret in

                    let peer = Oaks.discover client secret in
                    let peer_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

                    Fun.protect
                      ~finally:(fun () -> Unix.close peer_fd)
                      (fun () ->
                        Unix.connect peer_fd peer;
                        let key = Oaks.authed peer_fd secret in

                        File.recv path peer_fd key)
                | Cli.Help -> ()))
      in

      while (not (Atomic.get stop)) && not (Atomic.get is_done) do
        Unix.sleepf 0.05
      done;

      Atomic.set stop true;
      Domain.join task;
      Oaks.stop client discovery_task
