type mdns_client 
val create: int -> mdns_client
val start : mdns_client -> unit Domain.t
val stop : mdns_client -> 'a Domain.t -> 'a
