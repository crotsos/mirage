(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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

module Req : sig
    type op = Read | Write | Write_barrier | Flush | Unknown of int
    type seg = { gref : int32; first_sector : int; last_sector : int; }
    type t = {
      op : op;
      handle : int;
      id : int64;
      sector : int64;
      segs : seg array;
    }
    val segments_per_request : int
    val seg_size : int
    val idx_size : int
    val op_to_int : op -> int
    val op_of_int : int -> op
    val make_seg : seg -> Bitstring.bitstring
    val write_request : t -> string * int * 'a -> int64
    val read_request : string * int * int -> t
  end

module Res : sig
    type rsp = OK | Error | Not_supported | Unknown of int
    type t = { op : Req.op; st : rsp; }
    val read_response : string * int * int -> int64 * t
  end

type features = {
  barrier : bool;
  removable : bool;
  sector_size : int64;
  sectors : int64;
  readwrite: bool;
}

type t
type id = string
exception IO_error of string
val poll : t -> unit Lwt.t
val create : id:id -> Devices.blkif Lwt.t
val enumerate : unit -> id list Lwt.t
val read_page : t -> int64 -> Bitstring.t Lwt.t
val write_page : t -> int64 -> Bitstring.t -> unit Lwt.t
val read_512 : t -> int64 -> int64 -> Bitstring.bitstring array Lwt.t
