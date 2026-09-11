type mode = Help | Send of string | Recv of string * string option
type pending = SendPath | RecvSecret | RecvPath of string

exception MissingArg of string

let help =
  "oaks : file sharing over your LAN.\n\
   Usage:\n\
  \  oaks\n\
  \  oaks send <path>\n\
  \  oaks recv <secret> [path]\n\n\
   Commands:\n\
  \  help\n\
  \      Show this menu\n\
  \  send <path>\n\
  \      Send the file at <path>.\n\n\
  \  recv <secret> [path]\n\
  \      Receive using <secret>.\n\
  \      An optional path may be supplied as the receive destination.\n\n\
   With no arguments, this help message is shown."

let parse () =
  let rec loop mode pending args =
    match pending with
    | Some SendPath -> (
        match args with
        | path :: _rest -> Send path
        | [] -> raise @@ MissingArg "Missing send path")
    | Some RecvSecret -> (
        match args with
        | secret :: rest -> loop mode (Some (RecvPath secret)) rest
        | [] -> raise @@ MissingArg "Missing secret")
    | Some (RecvPath secret) -> (
        match args with
        | path :: _rest -> Recv (secret, Some path)
        | [] -> Recv (secret, None))
    | None -> (
        match args with
        | "send" :: rest -> loop mode (Some SendPath) rest
        | "recv" :: rest -> loop mode (Some RecvSecret) rest
        | "help" :: _rest -> Help
        | mode :: rest -> raise @@ Invalid_argument ("Invalid mode: " ^ mode)
        | [] -> mode)
  in

  Sys.argv |> Array.to_list |> List.tl |> loop Help None
