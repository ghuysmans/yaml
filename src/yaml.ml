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
	| Seq of node list
	| Map of (node * node) list

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

let mkMap map = mkNode (Map map)

let mkSeq list = mkNode (Seq list)

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

let seqStyle list =
	let isFlow =
		List.for_all
			(fun node -> isScalar node)
		list
	in
	if isFlow then
		Flow
	else
		Block

let mapStyle map =
	let isFlow =
		List.for_all
			(fun (key, value) -> isScalar key && isScalar value)
		map
	in
	if isFlow then
		Flow
	else
		Block

(* the buffer will be used by every output. *)
let b = Buffer.create 16384
let outc c = Buffer.add_char b c
let outs s = Buffer.add_string b s
let num_sp = 2

let ppScalar = function
	| Binary bin -> outc '"'; outs bin; outc '"'
	| Bool bool -> outs (string_of_bool bool)
	| Float f -> outs (string_of_float f)
	| Int i -> outs (string_of_int i)
	| Null -> outs "null"
	| Str s -> outc '"'; outs s; outc '"'

let ppIndent indent = for i = 1 to indent do outc ' ' done

let rec ppNewlines indent fn list =
	match list with
		| [] -> ()
		| [last] -> fn last
		| head :: rest ->
			fn head;
			outs "\n";
			ppIndent indent;
			ppNewlines indent fn rest

let rec ppCommas fn map =
	match map with
		| [] -> ()
		| [last] -> fn last
		| head :: rest ->
			fn head;
			outs ", ";
			ppCommas fn rest

let rec ppNode indent node =
	match node.kind with
		| Scalar scalar -> ppScalar scalar
		| Seq list -> ppSeq indent (seqStyle list) list
		| Map map -> ppMap indent (mapStyle map) map

and ppMap indent style map =
	match style with
		| Block ->
			let newIndent = indent + num_sp in
			ppNewlines indent
				(fun (key, value) ->
					ppNode newIndent key;
					if isScalar value then
						outs " : "
					else (
						outs " :\n";
						ppIndent newIndent
					);
					ppNode newIndent value)
			map
		| Flow ->
			outc '{';
			ppCommas
				(fun (key, value) ->
					ppNode indent key;
					outs " : ";
					ppNode indent value)
			map;
			outc '}'

and ppSeq indent style list =
	match style with
		| Block ->
			let newIndent = indent + num_sp in
			ppNewlines indent
				(fun node ->
					outs "- ";
					ppNode newIndent node)
			list
		| Flow ->
			outc '[';
			ppCommas
				(fun node -> ppNode indent node)
			list;
			outc ']'

let ppDoc oc doc =
	Buffer.clear b;
	outs "%YAML 1.2\n---\n";
	ppNode 0 doc.node;
	Buffer.output_buffer oc b;
	flush oc
