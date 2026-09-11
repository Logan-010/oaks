let create port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock 16;

  sock

let used_port sock =
  match Unix.getsockname sock with
  | Unix.ADDR_INET (_, port) -> port
  | _ -> invalid_arg "Not a valid addr type."
