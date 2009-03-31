(*****************************************************************************)
(* Yaml                                                                     *)
(* Copyright (c) 2007-2008, IETR/INSA of Rennes.                             *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* This software is governed by the CeCILL-B license under French law and    *)
(* abiding by the rules of distribution of free software. You can  use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B  *)
(* license as circulated by CEA, CNRS and INRIA at the following URL         *)
(* "http://www.cecill.info".                                                 *)
(*                                                                           *)
(* Matthieu WIPLIEZ <Matthieu.Wipliez@insa-rennes.fr                         *)
(*****************************************************************************)

open Printf

module SH =
  Hashtbl.Make
    (struct
			 type t = string
       let equal (a : string) (b : string) = (a = b)
       let hash (s : string) = Hashtbl.hash s
     end)

module SM = Map.Make
  (struct
		type t = string
    let compare = String.compare
  end)

type tag = {
	mutable shorthand : string;
	mutable full : string;
	mutable verbatim : bool;
}

type scalar =
	| Binary of string
	| Bool of bool
	| Float of float
	| Int of int
	| Null
	| Str of string

type style =
	| Block
	| Flow

type kind =
	| Scalar of scalar
	| Seq of style option * node list
	| Map of style option * (node * node) list

and node = {
	kind : kind;
	mutable tag : tag option;
}

type doc = {
	node : node;
	tags : tag SH.t;
}

let mkNode kind = {
	kind = kind;
	tag = None;
}

let mkBinary b = mkNode (Scalar (Binary b))

let mkBool b = mkNode (Scalar (Bool b))

let mkFloat f = mkNode (Scalar (Float f))

let mkInt i = mkNode (Scalar (Int i))

let mkNull () = mkNode (Scalar Null)

let mkStr s = mkNode (Scalar (Str s))

let mkMap ?style map = mkNode (Map (style, map))

let mkMapBlock map = mkMap ~style:Block map

let mkMapFlow map = mkMap ~style:Flow map

let mkSeq ?style list = mkNode (Seq (style, list))

let mkSeqBlock list = mkSeq ~style:Block list

let mkSeqFlow list = mkSeq ~style:Flow list

let mkDoc node = {
	node = node;
	tags = SH.create 30;
}

let isMap node =
	match node.kind with
		| Map _ -> true
		| _ -> false

let isScalar node =
	match node.kind with
		| Scalar _ -> true
		| _ -> false

let isSeq node =
	match node.kind with
		| Seq _ -> true
		| _ -> false

let ppScalar f = function
	| Binary b -> Format.fprintf f "%s" b
	| Bool b -> Format.fprintf f "%b" b
	| Float fl -> Format.fprintf f "%f" fl
	| Int i -> Format.fprintf f "%i" i
	| Null -> Format.fprintf f "null"
	| Str s -> Format.fprintf f "\"%s\"" s

let pp = Format.fprintf

let rec pp_newlines fn f list =
	match list with
		| [] -> ()
		| [last] -> fn last
		| head :: rest ->
			fn head;
			pp f "@ ";
			pp_newlines fn f rest

let rec ppNode f node =
	match node.kind with
		| Scalar scalar -> ppScalar f scalar
		| Seq (style, list) -> ppSeq f style list
		| Map (style, map) -> ppMap f style map

and ppMap f style map =
	let style =
		match style with
		| None -> Block
		| Some style -> style
	in
	match style with
		| Block ->
			pp f "@[<v>";
			pp_newlines
				(fun (key, value) ->
					if isScalar value then
						pp f "@[<hov>%a : %a@]" ppNode key ppNode value
					else
						pp f "@[<v2>@[<hov>%a : @]@ @[<hov>%a@]@]" ppNode key ppNode value)
			f map;
			pp f "@]"
		| Flow ->
			pp f "@[<hov>{";
			List.iter
				(fun (key, value) ->
					ppNode f key;
					pp f " : ";
					ppNode f value;
					pp f ", ")
			map;
			pp f "}@]"

and ppSeq f style list =
	let style =
		match style with
		| None -> Block
		| Some style -> style
	in
	match style with
		| Block ->
			pp f "@[<v>";
			pp_newlines
				(fun node ->
					pp f "@[<hov>- ";
					ppNode f node;
					pp f "@]")
			f list;
			pp f "@]"
		| Flow ->
			pp f "@[<hov>[";
			List.iter
				(fun node ->
					ppNode f node;
					pp f ", ")
			list;
			pp f "]@]"

let ppDoc f doc =
	pp f "@[<v>"; (* "@[<h>%%YAML@ 1.2@]@ " for YAML 1.2 compliance *)
	ppNode f doc.node;
	pp f "@]@?"
