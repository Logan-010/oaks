let mode = Cli.parse ()

let () =
  match mode with
  | Cli.Help -> prerr_endline Cli.help
  | _ ->
      let client = Oaks.create 1212 in
      let stop = Atomic.make false in

      ignore @@ Sys.signal Sys.sigint
      @@ Sys.Signal_handle (fun _ -> Atomic.set stop true);

      let task = Oaks.start client in

      while not @@ Atomic.get stop do
        ()
      done;

      Oaks.stop client task
