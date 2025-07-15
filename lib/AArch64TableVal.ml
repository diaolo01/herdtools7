(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module Attrs = struct
  module NormalAttrs = struct
    type sh = NSH | ISH | OSH
    let pp_sh = function | NSH -> "NSH" | ISH -> "ISH" | OSH -> "OSH"
    let compare_sh sh1 sh2 = compare sh1 sh2

    type ch = WB | WT | NC
    let pp_ch = function | WB -> "WB" | WT -> "WT" | NC -> "NC"
    let compare_ch c1 c2 = compare c1 c2

    type t = { sh : sh; ich : ch; och : ch; tagged: bool }
    let default = {sh=ISH; ich=WB; och=WB; tagged=false; }

    let eq a1 a2 = a1.sh = a2.sh && a1.ich = a2.ich && a1.och = a2.och &&
                   a1.tagged = a2.tagged

    let pp_type a = (if a.tagged then "Tagged" else "") ^ "Normal"
    let pp a = if a.tagged then "TaggedNormal" else
        sprintf "%s,%s,%s,%s" (pp_type a)
          (pp_sh a.sh) ("i" ^ pp_ch a.ich) ("o" ^ pp_ch a.och)

    let compare a1 a2 =
      match compare_sh a1.sh a2.sh with
      | 0 -> begin
          match compare_ch a1.ich a2.ich with
          | 0 -> compare_ch a1.och a2.och
          | n -> n
        end
      | n -> n

    let as_list a = [ pp_type a; pp_sh a.sh; "i" ^ pp_ch a.ich; "o" ^ pp_ch a.och ]

    let check attrs =
      let sh_attrs =
        List.filter (
          function
            | "NSH" | "Non-shareable"
            | "ISH" | "Inner-shareable"
            | "OSH" | "Outer-shareable" -> true
            | _ -> false) attrs in
      if List.length sh_attrs > 1 then
         Warn.user_error "Conflicting shareability" ;
      let ich_attrs =
        List.filter (
          function
          | "iWB" | "Inner-write-back"
          | "iWT" | "Inner-write-through"
          | "iNC" | "Inner-non-cacheable" -> true
          | _ -> false) attrs in
      if List.length ich_attrs > 1 then
        Warn.user_error "Conflicting Inner-Cacheability" ;
      let och_attrs =
        List.filter (
          function
          | "oWB" | "Outer-write-back"
          | "oWT" | "Outer-write-through"
          | "oNC" | "Outer-non-cacheable" -> true
          | _ -> false) attrs in
      if List.length och_attrs > 1 then
        Warn.user_error "Conflicting Outer-Cacheability"

    let parse s attrs =
      match s with
      | "NSH" | "Non-shareable" -> {attrs with sh=NSH}
      | "ISH" | "Inner-shareable" -> {attrs with sh=ISH}
      | "OSH" | "Outer-shareable" -> {attrs with sh=OSH}
      | "iWB" | "Inner-write-back"-> {attrs with ich=WB}
      | "iWT" | "Inner-write-through" -> {attrs with ich=WT}
      | "iNC" | "Inner-non-cacheable" -> {attrs with ich=NC}
      | "oWB" | "Outer-write-back" -> {attrs with och=WB}
      | "oWT" | "Outer-write-through" -> {attrs with och=WT}
      | "oNC" | "Outer-non-cacheable" -> {attrs with och=NC}
      | "Normal" -> attrs
      | "TaggedNormal" -> {attrs with tagged=true}
      | _ -> Warn.user_error "Invalid memory attribute %s" s

    let rec of_list ?(attrs=default) = function
      | [] -> attrs
      | h::l -> of_list ~attrs:(parse h attrs) l

    let is_tagged a = a.tagged

    let as_kvm_symbols a =
      match a with
      | {sh; ich; och; tagged=false} when ich = och ->
        [sprintf "attr_Normal_i%s_o%s" (pp_ch ich) (pp_ch och); "attr_" ^ (pp_sh sh)]
      | _ -> Warn.user_error "Memory attribute not supported in kvm-unit-tests"

  end

  module DeviceAttrs = struct
    type t = GRE | NGRE | NGnRE | NGnRnE
    let eq a1 a2 = a1 = a2

    let pp_attr = function
      | GRE -> "GRE"
      | NGRE -> "nGRE"
      | NGnRE -> "nGnRE"
      | NGnRnE -> "nGnRnE"

    let pp d = sprintf "Device-%s" (pp_attr d)

    let compare a1 a2 = compare a1 a2
    let as_list a = [ pp a ]
    let of_list = function
      | ["Device-GRE"] -> Some GRE
      | ["Device-nGRE"] -> Some NGRE
      | ["Device-nGnRE"] -> Some NGnRE
      | ["Device-nGnRnE"] -> Some NGnRnE
      | _ -> None

    let as_kvm_symbols a =
      match a with
      | GRE | NGnRE | NGnRnE -> ["attr_Device_" ^ (pp_attr a)]
      | _ -> Warn.user_error "Device attribute not supported in kvm-unit-tests"
  end

  type t = Normal of NormalAttrs.t | Device of DeviceAttrs.t

  (* By default we assume the attributes of the memory malloc would
     return on Linux. This is architecture specific, however, for now,
     translation is supported only for AArch64. *)
  let default = Normal NormalAttrs.default

  let compare a1 a2 =
    match a1, a2 with
    | Normal n1, Normal n2 -> NormalAttrs.compare n1 n2
    | Device d1, Device d2 -> DeviceAttrs.compare d1 d2
    | Normal _, Device _ -> -1
    | Device _, Normal _ -> 1

  let eq a1 a2 = match a1, a2 with
    | Device d1, Device d2 -> DeviceAttrs.eq d1 d2
    | Normal n1, Normal n2 -> NormalAttrs.eq n1 n2
    | _, _ -> false

  let pp = function
    | Normal a -> NormalAttrs.pp a
    | Device a -> DeviceAttrs.pp a

  let as_list = function
    | Normal a -> NormalAttrs.as_list a
    | Device a -> DeviceAttrs.as_list a

  let is_tagged = function
    | Normal n -> NormalAttrs.is_tagged n
    | Device _ -> false

  let of_list l =
    match DeviceAttrs.of_list l with
    | Some d -> Device d
    | None ->
      NormalAttrs.check l ;
      Normal (NormalAttrs.of_list l)

  let as_kvm_symbols = function
    | Normal n -> NormalAttrs.as_kvm_symbols n
    | Device d -> DeviceAttrs.as_kvm_symbols d
end


type t = {
  oa : OutputAddress.t;
  valid : int;
  attrs: Attrs.t;
  }

let eq_props p1 p2 =
  Misc.int_eq p1.valid p2.valid &&
  Attrs.eq p1.attrs p2.attrs

(* Let us abstract... *)

and same_oa {oa=oa1; _} {oa=oa2; _} = OutputAddress.eq oa1 oa2

let get_attrs {attrs;_ } = Attrs.as_list attrs

let prot_default =
  { oa=OutputAddress.PTE "";
    valid=1; attrs=Attrs.default; }

let default s = { prot_default with  oa=OutputAddress.PTE s; }

let pp_field ok pp eq ac p k =
  let f = ac p in if not ok && eq f (ac prot_default) then k else pp f::k

(* hexa arg is ignored, as it would complicate normalisation *)
let pp_int_field _hexa ok name =
  let pp_int = sprintf "%d" in
  pp_field ok (fun v -> sprintf "%s:%s" name (pp_int v)) Misc.int_eq

let pp_valid hexa ok = pp_int_field hexa ok "valid" (fun p -> p.valid)
and pp_attrs ok = pp_field ok (fun a -> Attrs.pp a) Attrs.eq (fun p -> p.attrs)

let is_default t =  eq_props prot_default t

let pp_fields hexa showall p k =
  pp_valid hexa showall p k

let do_pp hexa showall old_oa p =
  let k = pp_attrs false p [] in
  let k = pp_fields hexa showall p k in
  let k =
    sprintf "oa:%s"
      ((if old_oa then OutputAddress.pp_old
        else OutputAddress.pp) p.oa)::k  in
  let fs = String.concat ", " k in
  sprintf "(%s)" fs

(* By default pp does not list fields whose value is default *)
let pp hexa = do_pp hexa false false
(* For initial values dumped for hashing, pp_hash is different,
   for not altering hashes as much as possible *)
let pp_v = pp false
let pp_hash = do_pp false true true

let my_int_of_string s v =
  let v = try int_of_string v with
    _ -> Warn.user_error "PTE field %s should be an integer" s
  in v

let add_field k v p =
  match k with
  | "valid" -> { p with valid = my_int_of_string k v }
  | _ ->
      Warn.user_error "Illegal AArch64 table descriptor property %s" k

let tr p =
  let open ParsedPteVal in
  let r = {prot_default with attrs=Attrs.of_list (StringSet.elements p.p_attrs)} in
  let r =
    match p.p_oa with
    | None -> r
    | Some oa -> { r with oa; } in
  let r = StringMap.fold add_field p.p_kv r in
  r

let pp_norm p =
  let n = tr p in
  pp_v n

let lex_compare c1 c2 x y  = match c1 x y with
| 0 -> c2 x y
| r -> r

let compare =
  let cmp = (fun p1 p2 -> Misc.int_compare p1.valid p2.valid) in
  let cmp =
    lex_compare (fun p1 p2 -> OutputAddress.compare p1.oa p2.oa) cmp in
  let cmp =
    lex_compare (fun p1 p2 -> Attrs.compare p1.attrs p2.attrs) cmp in
  cmp

let eq p1 p2 = OutputAddress.eq p1.oa p2.oa && eq_props p1 p2

(* For litmus *)

(* Those lists must of course match one with the other *)
let fields = ["valid";]
and default_fields =
  let p = prot_default in
  let ds = [p.valid;] in
  List.map (Printf.sprintf "%i") ds

let norm =
  let default =
    try
      List.fold_right2
        (fun k v -> StringMap.add k v)
        fields
        default_fields
        StringMap.empty
    with Invalid_argument _ -> assert false in
  fun kvs ->
  StringMap.fold
    (fun k v m ->
      try
        let v0 = StringMap.find k default in
        if Misc.string_eq v v0 then m
        else StringMap.add k v m
      with
      | Not_found -> StringMap.add k v m)
    kvs StringMap.empty

let dump_pack pp_oa p =
  sprintf
    "pack_pack(%s,%d)"
    (pp_oa (OutputAddress.pp_old p.oa))
    p.valid

let as_physical p = OutputAddress.as_physical p.oa

let as_flags p =
  if is_default p then None
  else Some "msk_valid"

let attrs_as_kvm_symbols p =
  Attrs.as_kvm_symbols p.attrs
