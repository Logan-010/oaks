type mode =
  | Help
  | Send of string * string option
  | Recv of string * string option

type cli = { port : int; interface : string; mode : mode }

val help : string
val parse : unit -> cli
