type uint16 = Ofpacket.uint16
type uint32 = Ofpacket.uint32
type uint64 = Ofpacket.uint64
type byte = Ofpacket.byte
type eaddr = Ofpacket.eaddr
type port = uint16
type cookie = uint64
type device = string

module Switch :
  sig
    type t
  end
val process_frame : OS.Netif.id -> Cstruct.buf -> unit Lwt.t
val add_flow: Ofpacket.Match.t -> Ofpacket.Flow.action list -> unit
val add_port : Switch.t -> Net.Manager.t -> OS.Netif.t -> unit 
val listen : Net.Manager.t -> Net.Nettypes.ipv4_src -> 
  (Net.Manager.t -> Switch.t -> unit Lwt.t) -> unit Lwt.t
