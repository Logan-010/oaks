type mode =
  | Help
  | Send of string * string option
  | Recv of string * string option

type pending =
  | SendPath
  | SendSecret of string
  | RecvSecret
  | RecvPath of string
  | Interface
  | Port

exception MissingArg of string

type cli = { port : int; interface : string; mode : mode }

let help =
  "OAKS: file sharing over your LAN.\n\n\
   Usage:\n\
  \  oaks\n\
  \  oaks (flags)\n\
  \  oaks send <path>\n\
  \  oaks recv <secret> [path]\n\n\
   Flags\n\
  \  -i / --interface <interface>\n\
  \    Set interface to listen on\n\
  \  -p / --port <port>\n\
  \    Set port to listen on\n\n\
   Commands:\n\
  \  help\n\
  \    Show this menu\n\
  \  send <path> [secret]\n\
  \    Send the file at <path>.\n\
  \    An optional secret may be supplied to set a custom secret.\n\
  \  recv <secret> [path]\n\
  \    Receive using <secret>.\n\
  \    An optional path may be supplied as the receive destination.\n\n\
   With no arguments, this help message is shown."

let parse () =
  let rec loop cli pending args =
    match pending with
    | Some SendPath -> (
        match args with
        | path :: rest -> loop cli (Some (SendSecret path)) rest
        | [] -> raise @@ MissingArg "Missing send path")
    | Some (SendSecret path) -> (
        match args with
        | secret :: _rest -> { cli with mode = Send (path, Some secret) }
        | [] -> { cli with mode = Send (path, None) })
    | Some RecvSecret -> (
        match args with
        | secret :: rest -> loop cli (Some (RecvPath secret)) rest
        | [] -> raise @@ MissingArg "Missing secret")
    | Some (RecvPath secret) -> (
        match args with
        | path :: _rest -> { cli with mode = Recv (secret, Some path) }
        | [] -> { cli with mode = Recv (secret, None) })
    | Some Interface -> (
        match args with
        | interface :: rest -> loop { cli with interface } None rest
        | [] -> raise @@ MissingArg "Missing interface")
    | Some Port -> (
        match args with
        | port :: rest -> loop { cli with port = int_of_string port } None rest
        | [] -> raise @@ MissingArg "Missing port")
    | None -> (
        match args with
        | "send" :: rest -> loop cli (Some SendPath) rest
        | "recv" :: rest -> loop cli (Some RecvSecret) rest
        | "help" :: _rest | "--help" :: _rest | "-h" :: _rest ->
            { cli with mode = Help }
        | "--interface" :: rest | "-i" :: rest -> loop cli (Some Interface) rest
        | cli :: rest -> raise @@ Invalid_argument ("Invalid cli: " ^ cli)
        | [] -> cli)
  in

  Sys.argv |> Array.to_list |> List.tl
  |> loop { mode = Help; port = 0; interface = "0.0.0.0" } None
