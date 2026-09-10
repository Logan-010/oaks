type mdns_client 
val default_client : unit -> mdns_client
val start : mdns_client -> unit Domain.t
val stop : mdns_client -> 'a Domain.t -> 'a
