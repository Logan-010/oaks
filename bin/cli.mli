type mode = Help | Send of string | Recv of string * string option

val help : string
val parse : unit -> mode
