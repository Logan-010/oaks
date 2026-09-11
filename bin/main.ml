let client = Oaks.create 1212
let stop = Atomic.make false

let () =
  ignore @@ Sys.signal Sys.sigint
  @@ Sys.Signal_handle (fun _ -> Atomic.set stop true);

  let task = Oaks.start client in

  while not @@ Atomic.get stop do
    ()
  done;

  Oaks.stop client task
