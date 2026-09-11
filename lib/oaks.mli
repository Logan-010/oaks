type mdns_client 
val create: int -> mdns_client
val start : mdns_client -> unit Domain.t
val stop : mdns_client -> 'a Domain.t -> 'a
val auth : Unix.file_descr -> Bytes.t -> Bytes.t
val authed : Unix.file_descr -> Bytes.t -> Bytes.t
val secret : mdns_client -> Bytes.t
val id : mdns_client -> string
val discover : mdns_client -> Bytes.t -> Unix.sockaddr
