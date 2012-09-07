(*
 * Copyright (c) 2011 Charalampos Rotsos <cr409@cl.cam.ac.uk> 
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

open Net
open Net.Nettypes
open Cstruct

exception Unparsable of Cstruct.buf

type lldp_tlv_types = 
  | LLDP_TYPE_END            
  | LLDP_TYPE_CHASSIS_ID    
  | LLDP_TYPE_PORT_ID       
  | LLDP_TYPE_TTL           
  | LLDP_TYPE_PORT_DESCR    
  | LLDP_TYPE_SYSTEM_NAME   
  | LLDP_TYPE_SYSTEM_DESCR  
  | LLDP_TYPE_SYSTEM_CAP    
  | LLDP_TYPE_MGMT_ADDR     

type lldp_tvl =
  | Tlv_chassis_id_chassis_comp of string
  | Tlv_chassis_id_intf_alias of string
  | Tlv_chassis_id_port_comp of string
  | Tlv_chassis_id_mac of ethernet_mac
  | Tlv_chassis_id_net of ipv4_addr 
  | Tlv_chassis_id_intf_name of string
  | Tlv_chassis_id_local of string
  | Tlv_port_id_intf_alias of string
  | Tlv_port_id_port_comp of string
  | Tlv_port_id_mac of ethernet_mac
  | Tlv_port_id_net of ipv4_addr
  | Tlv_port_id_intf_name of string
  | Tlv_port_id_circ_id of string
  | Tlv_port_id_local of string
  | Tlv_ttl of int
  | Tlv_end 
  | Tlv of lldp_tlv_types * string
  | Tlv_unk of int * string

val parse_lldp_tlvs: buf -> lldp_tvl list
val marsal_lldp_tlvs: ethernet_mac -> lldp_tvl list -> buf -> int 
