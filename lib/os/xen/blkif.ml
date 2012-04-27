(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf

(* Request of a page *)
module Req = struct
  type op = 
    |Read
    |Write
    |Write_barrier
    |Flush
    |Unknown of int

  type seg = {
    gref: int32;
    first_sector: int;
    last_sector: int;
  }

  (* Defined in include/xen/io/blkif.h : blkif_request_t *)
  type t = {
    op: op;
    handle: int;
    id: int64;
    sector: int64;
    segs: seg array;
  }

  let segments_per_request = 11 (* Defined by the protocol *)
  let seg_size = 8 (* bytes, 6 +2 struct padding *)
  let idx_size =  (* total size of a request structure, in bytes *)
    1 + (* operation *)
    1 + (* nr_segments *)
    2 + (* handle *)
    4 + (* struct padding to 64-bit *)
    8 + (* id *)
    8 + (* sector number *)
    (seg_size * segments_per_request) 

  (* Defined in include/xen/io/blkif.h, BLKIF_REQ_* *)
  let op_to_int = function
    |Read->0 |Write->1 |Write_barrier->2 |Flush->3 |Unknown n -> n

  let op_of_int = function
    |0->Read |1->Write |2->Write_barrier |3->Flush |n->Unknown n

  (* Marshal an individual segment request *)
  let make_seg seg =
    BITSTRING { seg.gref:32:littleendian; seg.first_sector:8:littleendian;
      seg.last_sector:8:littleendian; 0:16 }

  (* Write a request to a slot in the shared ring. Could be optimised a little
     more to minimise allocation, if it matters. *)
  let write_request req (bs,bsoff,bslen) =
    let op = op_to_int req.op in
    let nr_segs = Array.length req.segs in
    let segs = Bitstring.concat (Array.to_list (Array.map make_seg req.segs)) in
    let reqbuf,_,reqlen = BITSTRING {
      op:8:littleendian; nr_segs:8:littleendian;
      req.handle:16:littleendian; 0l:32; req.id:64:littleendian;
      req.sector:64:littleendian; segs:-1:bitstring } in
    String.blit reqbuf 0 bs (bsoff/8) (reqlen/8);
    req.id

  (* Read a request out of a bitstring; to be used by the Ring.Back for serving
     requests, so this is untested for now *)
  let read_request bs =
    bitmatch bs with
    | { op:8:littleendian; nr_segs:8:littleendian; handle:16:littleendian;
        id:64:littleendian; sector:64:littleendian; segs:-1:bitstring } ->
          let seglen = seg_size * 8 in
          let segs = Array.init nr_segs (fun i ->
            bitmatch (Bitstring.subbitstring segs (i*seglen) seglen) with
            | { gref:32:littleendian; first_sector:8:littleendian;
                last_sector:8:littleendian } ->
                 {gref; first_sector; last_sector }
          ) in
          let op = op_of_int op in
          { op; handle; id; sector; segs }
end

module Res = struct

  type rsp = 
   |OK
   |Error
   |Not_supported
   |Unknown of int
 
  (* Defined in include/xen/io/blkif.h, blkif_response_t *)
  type t = {
    op: Req.op;
    st: rsp;
  }

  (* Defined in include/xen/io/blkif.h, BLKIF_RSP_* *)
  let read_response bs =
    (bitmatch bs with
    | { id:64:littleendian; op:8:littleendian; _:8;
        st:16:littleendian } ->
          let op = Req.op_of_int op in
          let st = match st with 
            |0 -> OK |0xffff -> Error |0xfffe -> Not_supported |n -> Unknown n in
          (id, { op; st }))
end

type features = {
  barrier: bool;
  removable: bool;
  sector_size: int64; (* stored as int64 for convenient division *)
  sectors: int64;
  readwrite: bool;
}

type t = {
  backend_id: int;
  backend: string;
  vdev: int;
  ring: (Res.t,int64) Ring.Front.t;
  gnts: Gnttab.r list;
  evtchn: int;
  features: features;
}

type id = string
exception IO_error of string

(** Set of active block devices *)
let devices : (id, t) Hashtbl.t = Hashtbl.create 1

(* Allocate a ring, given the vdev and backend domid *)
let alloc ~order (num,domid) =
  let name = sprintf "Blkif.%d" num in
  let idx_size = Req.idx_size in (* bigger than res *)
  lwt (rx_gnts, sring) = Ring.init ~domid ~order ~idx_size ~name in
  let fring = Ring.Front.init ~sring in
  return (rx_gnts, fring)

(* Thread to poll for responses and activate wakeners *)
let rec poll t =
  Activations.wait t.evtchn >>
  let () = Ring.Front.poll t.ring (Res.read_response) in
  poll t

(* Given a VBD ID and a backend domid, construct a blkfront record *)
let plug (id:id) =
  lwt vdev = try return (int_of_string id)
    with _ -> fail (Failure "invalid vdev") in
  printf "Blkfront.create; vdev=%d\n%!" vdev;
  let node = sprintf "device/vbd/%d/%s" vdev in
  lwt backend_id = Xs.(t.read (node "backend-id")) in
  lwt backend_id = try_lwt return (int_of_string backend_id)
    with _ -> fail (Failure "invalid backend_id") in
  lwt backend = Xs.(t.read (node "backend")) in

  let backend_read fn default k =
    let backend = sprintf "%s/%s" backend in
      try_lwt
        lwt s = Xs.(t.read (backend k)) in
        return (fn s)
      with exn -> return default in

  (* The backend can advertise a multi-page ring: *)
  lwt backend_max_ring_page_order = backend_read int_of_string 0 "max-ring-page-order" in
  if backend_max_ring_page_order = 0
  then printf "Blkback can only use a single-page ring\n%!"
  else printf "Blkback advertises multi-page ring (size 2 ** %d pages)\n%!" backend_max_ring_page_order;

  let our_max_ring_page_order = 2 in (* 4 pages *)
  let ring_page_order = min our_max_ring_page_order backend_max_ring_page_order in
  printf "Negotiated a %s\n%!" (if ring_page_order = 0 then "singe-page ring" else sprintf "multi-page ring (size 2 ** %d pages)" ring_page_order);

  lwt (gnts, ring) = alloc ~order:ring_page_order (vdev,backend_id) in
  let evtchn = Evtchn.alloc_unbound_port backend_id in

  let ring_info =
    (* The new protocol writes (ring-refN = G) where N=0,1,2 *)
    let rfs = snd(List.fold_left (fun (i, acc) g ->
      i + 1, ((sprintf "ring-ref%d" i, Gnttab.to_string g) :: acc)
    ) (0, []) gnts) in
    if ring_page_order = 0
    then [ "ring-ref", Gnttab.to_string (List.hd gnts) ] (* backwards compat *)
    else [ "ring-page-order", string_of_int ring_page_order ] @ rfs in
  let info = [
    "event-channel", string_of_int evtchn;
    "protocol", "x86_64-abi";
    "state", Xb.State.(to_string Connected)
  ] @ ring_info in
  Xs.(transaction t (fun xst ->
    let wrfn k v = xst.Xst.write (node k) v in
    Lwt_list.iter_s (fun (k, v) -> wrfn k v) info
  )) >>
  lwt monitor_t = Xs.(monitor_path Xs.t
    (sprintf "%s/state" backend, "XXX") 20. 
    (fun (k,_) ->
        lwt state = try_lwt Xs.t.Xs.read k with _ -> return "" in
	    return Xb_state.(of_string state = Connected)
	)) in
  (* Read backend features *)
  lwt features =
    lwt state = backend_read (Xb_state.of_string) Xb_state.Unknown "state" in
    printf "state=%s\n%!" (Xb_state.prettyprint state);
    lwt barrier = backend_read ((=) "1") false "feature-barrier" in
    lwt removable = backend_read ((=) "1") false "removable" in
    lwt sectors = backend_read Int64.of_string (-1L) "sectors" in
    lwt sector_size = backend_read Int64.of_string 0L "sector-size" in
    lwt readwrite = backend_read (fun x -> x = "w") false "mode" in
    return { barrier; removable; sector_size; sectors; readwrite }
  in
  printf "Blkfront features: barrier=%b removable=%b sector_size=%Lu sectors=%Lu\n%!" 
    features.barrier features.removable features.sector_size features.sectors;
  Evtchn.unmask evtchn;
  let t = { backend_id; backend; vdev; ring; gnts; evtchn; features } in
  Hashtbl.add devices id t;
  (* Start the background poll thread *)
  let _ = poll t in
  return t

(* Unplug shouldn't block, although the Xen one might need to due
   to Xenstore? XXX *)
let unplug id =
  Console.log (sprintf "Blkif.unplug %s: not implemented yet" id);
  ()

(** Return a list of valid VBDs *)
let enumerate () =
  try_lwt Xs.(t.directory "device/vbd") with Xb.Noent -> return []

let create fn =
  let th,_ = Lwt.task () in
  Lwt.on_cancel th (fun _ -> Hashtbl.iter (fun id _ -> unplug id) devices);
  lwt ids = enumerate () in
  let pt = Lwt_list.iter_p (fun id ->
    lwt t = plug id in
    fn id t) ids in
  th <?> pt

(* Read a single page from disk. The offset must be sector-aligned *)
let read_page t offset =
  let sector = Int64.div offset t.features.sector_size in
  Io_page.with_page
    (fun page ->
      Gnttab.with_ref
        (fun r ->
          Gnttab.with_grant ~domid:t.backend_id ~perm:Gnttab.RW r page
            (fun () ->
	      let gref = Gnttab.to_int32 r in
	      let id = Int64.of_int32 (Gnttab.to_int32 r) in
              let segs =[| { Req.gref; first_sector=0; last_sector=7 } |] in
              let req = Req.({op=Req.Read; handle=t.vdev; id; sector; segs}) in
              let res = Ring.Front.push_request_and_wait t.ring (Req.write_request req) in
              if Ring.Front.push_requests_and_check_notify t.ring then
                Evtchn.notify t.evtchn;
              lwt res = res in
              Res.(match res.st with
              |Error -> fail (IO_error "read")
              |Not_supported -> fail (IO_error "unsupported")
              |Unknown _ -> fail (IO_error "unknown error")
              |OK ->
				  let copy = Bitstring.bitstring_of_string (Bitstring.string_of_bitstring (Io_page.to_bitstring page)) in
				  return copy)
            )
        )
    )

(* Write a single page to disk.
   Offset is the sector number, which must be sector-aligned
   Page must be an Io_page *)
let write_page t offset page =
  let sector = Int64.div offset t.features.sector_size in
  if not t.features.readwrite
  then fail (IO_error "read-only")
  else Gnttab.with_ref
    (fun r ->
      Gnttab.with_grant ~domid:t.backend_id ~perm:Gnttab.RW r (Io_page.of_bitstring page)
        (fun () ->
          let gref = Gnttab.to_int32 r in
          let id = Int64.of_int32 gref in
          let segs =[| { Req.gref; first_sector=0; last_sector=7 } |] in
          let req = Req.({op=Req.Write; handle=t.vdev; id; sector; segs}) in
          let res = Ring.Front.push_request_and_wait t.ring (Req.write_request req) in
          if Ring.Front.push_requests_and_check_notify t.ring then
            Evtchn.notify t.evtchn;
          let open Res in
          lwt res = res in
          Res.(match res.st with
          |Error -> fail (IO_error "write")
          |Not_supported -> fail (IO_error "unsupported")
          |Unknown _ -> fail (IO_error "unknown error")
          |OK -> return ())
        )
    )

(* Issues a single request to read from [start_sector + start_offset] to [end_sector - end_offset]
   where: [start_sector] and [end_sector] are page-aligned; and the total number of pages will fit
   in a single request. *)
let single_512_byte_page_aligned_read t (start_sector, start_offset) (end_sector, end_offset) =
  let len = Int64.(to_int (div (sub end_sector start_sector) 8L)) in  
  if len > 11 then
    fail (Failure (sprintf "len > 11 (%Lu, %u) (%Lu, %u)" start_sector start_offset end_sector end_offset))
  else 
    Io_page.with_pages len
      (fun pages ->
	Gnttab.with_refs len
          (fun rs ->
            Gnttab.with_grants ~domid:t.backend_id ~perm:Gnttab.RW rs pages
	      (fun () ->
		let segs = Array.mapi
		  (fun i r ->
		    let first_sector = match i with
		      |0 -> start_offset
		      |_ -> 0 in
		    let last_sector = match i with
		      |n when n == len-1 -> end_offset
		      |_ -> 7 in
		    let gref = Gnttab.to_int32 r in
		    { Req.gref; first_sector; last_sector }
		  ) (Array.of_list rs) in
		let id = Int64.of_int32 (Gnttab.to_int32 (List.hd rs)) in
		let req = Req.({ op=Read; handle=t.vdev; id; sector=start_sector; segs }) in
		let res = Ring.Front.push_request_and_wait t.ring (Req.write_request req) in
		if Ring.Front.push_requests_and_check_notify t.ring then
		  Evtchn.notify t.evtchn;
		let open Res in
		    lwt res = res in
		match res.st with
		  |Error -> fail (IO_error "read")
		  |Not_supported -> fail (IO_error "unsupported")
		  |Unknown x -> fail (IO_error "unknown error")
		  |OK ->
		    (* Get the pages, and convert them into Istring views *)
		    let pages = Array.mapi
		      (fun i page ->
			let bs = Io_page.to_bitstring page in
			let start_offset = match i with
			  |0 -> start_offset * 512
			  |_ -> 0 in
			let end_offset = match i with
			  |n when n = len-1 -> (end_offset + 1) * 512
			  |_ -> 4096 in
			let bytes = end_offset - start_offset in
			Bitstring.subbitstring bs (start_offset * 8) (bytes * 8)
		      ) (Array.of_list pages) in
		    return pages
	      )
	  )
      )

(* Reads [num_sectors} 512-byte sectors starting at [sector] recursively. *)
let rec read_512_chunks t sector num_sectors =
  assert (sector >= 0L);
  assert (num_sectors > 0L);
  (* Round down the starting sector in order to get a page aligned sector *)
  let start_sector = Int64.(mul 8L (div sector 8L)) in
  let start_offset = Int64.(to_int (sub sector start_sector)) in
  (* Round up the ending sector to the page boundary *)
  let end_sector = Int64.(mul 8L (div (add (add sector num_sectors) 7L) 8L)) in
  (* Calculate number of sectors needed *)
  let total_sectors_needed = Int64.(sub end_sector start_sector) in
  (* Maximum of 11 segments per request; 1 page (8 sectors) per segment so: *)
  let total_sectors_possible = min 88L total_sectors_needed in

  let possible_end_sector = Int64.add start_sector total_sectors_possible in
  let end_offset = min 7 (Int64.(to_int (sub 7L (sub possible_end_sector (add sector num_sectors))))) in

  lwt pages = single_512_byte_page_aligned_read t (start_sector, start_offset) (possible_end_sector, end_offset) in
  if total_sectors_possible < total_sectors_needed
  then
    let num_sectors = Int64.(sub num_sectors (sub total_sectors_possible (of_int start_offset))) in
    lwt rest = Int64.(read_512_chunks t (add start_sector total_sectors_possible) num_sectors) in
    return (Array.concat [ pages; rest ])
  else
    return pages

(* Reads [num_sectors] starting at [sector], returning an array of bitstrings *)
let read_512 t sector num_sectors =
  lwt pages = read_512_chunks t sector num_sectors in
  (* Sanity check that we read the right size of data *)
  let total_size = Int64.of_int (Array.fold_left (fun acc bs -> Bitstring.bitstring_length bs / 8 + acc) 0 pages) in
  let size_should_be = Int64.mul 512L num_sectors in
  if total_size <> size_should_be then begin
    printf "total_size = %Ld\n" total_size;
    printf "size_should_be = %Ld\n%!" size_should_be;
    fail (IO_error "size")
  end else
    return pages

let create ~id : Devices.blkif Lwt.t =
  printf "Xen.Blkif: create %s\n%!" id;
  lwt dev = plug id in
  printf "Xen.Blkif: success\n%!";
  return (object
    method id = id
    method read_page = read_page dev
    method read_512 = read_512 dev
    method write_page = write_page dev
    method sector_size = 4096
    method size = Int64.mul dev.features.sectors dev.features.sector_size
    method readwrite = dev.features.readwrite
    method ppname = sprintf "Xen.blkif:%s" id
    method destroy = unplug id
  end)

(* Register Xen.Blkif provider with the device manager *)
let _ =
  let plug_mvar = Lwt_mvar.create_empty () in
  let unplug_mvar = Lwt_mvar.create_empty () in
  let provider = object(self)
     method id = "Xen.Blkif"
     method plug = plug_mvar 
     method unplug = unplug_mvar
     method create ~deps ~cfg id =
	  (* no cfg required: we will check xenstore instead *)
      lwt blkif = create ~id in
      let entry = Devices.({
        provider=self; 
        id=self#id; 
        depends=[];
        node=Blkif blkif }) in
      return entry
  end in
  Devices.new_provider provider;
  (* Iterate over the plugged in VBDs and plug them in *)
  Main.at_enter (fun () ->
    (* Hack to let console attach before crash :) *)
    Time.sleep 1.0 >>
    lwt ids = enumerate () in
	let vbds = List.map (fun id ->
		{ Devices.p_dep_ids = []; p_cfg = []; p_id = id }
	) ids in
    Lwt_list.iter_s (Lwt_mvar.put plug_mvar) vbds
  )

