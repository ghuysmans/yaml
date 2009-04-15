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

(*****************************************************************************)
(* Unicode stuff *)

exception Malformed                 (* for character stream, internal only. *)

let utf8_str str nb uchar =
	if uchar <= 0x7F then (
		(* char = 0xxx xxxx => 0xxx xxxx *)
		str.[nb] <- char_of_int uchar;
		nb + 1
	) else if uchar <= 0x07FF then (
		(* char = 0000 0yyy, yyxx xxxx => 110y yyyy, 10xx xxxx *)
		str.[nb + 0] <- char_of_int (0xC0 lor (uchar lsr 6));
		str.[nb + 1] <- char_of_int (0x80 lor (uchar land 0x3F));
		nb + 2
	) else if uchar <= 0xFFFF then (
		(* char = zzzz yyyy, yyxx xxxx => 1110 zzzz, 10yy yyyy, 10xx xxxx *)
		str.[nb + 0] <- char_of_int (0xE0 lor (uchar lsr 12));
		str.[nb + 1] <- char_of_int (0x80 lor ((uchar lsr 6) land 0x3F));
		str.[nb + 2] <- char_of_int (0x80 lor (uchar land 0x3F));
		nb + 3
	) else if uchar <= 0x1FFFFF then (
		(* char = 000u uuuu, zzzz yyyy, yyxx xxxx => *)
		(* 1111 0uuu, 10uu zzzz, 10yy yyyy, 10xx xxxx *)
		str.[nb + 0] <- char_of_int (0xF0 lor (uchar lsr 18));
		str.[nb + 1] <- char_of_int (0x80 lor ((uchar lsr 12) land 0x3F));
		str.[nb + 2] <- char_of_int (0x80 lor ((uchar lsr 6) land 0x3F));
		str.[nb + 3] <- char_of_int (0x80 lor (uchar land 0x3F));
		nb + 4
	) else
		raise Malformed

let utf8 array =
	(* this way we never need to reallocate. *)
	let str = String.create (4 * Array.length array) in
	let num_bytes =
		Array.fold_left
			(fun nb uc -> utf8_str str nb uc)
			0 array
	in
	(* now we get the subset we really need *)
	String.sub str 0 num_bytes

(* The unicode lexer code below is from Xmlm by Daniel C. Bünzli. *)

(* Unicode character lexers *)

let utf8_len = [|        (* Char byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

(*****************************************************************************)

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

(* this function prints the given string using double-quoted, *)
(* single-quoted, or plain style according to the characters *)
(* the string contains. *)
let ppString s =
	
	outc '"'; outs s; outc '"'

let ppScalar = function
	| Binary bin -> outc '"'; outs bin; outc '"'
	| Bool bool -> outs (string_of_bool bool)
	| Float f -> outs (string_of_float f)
	| Int i -> outs (string_of_int i)
	| Null -> outs "null"
	| Str s -> ppString s

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
	outs "\xEF\xBB\xBF"; (* UTF-8 BOM *)
	outs "%YAML 1.2\n---\n"; (* YAML 1.2 declaration *)
	ppNode 0 doc.node;
	Buffer.output_buffer oc b;
	flush oc
