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
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; (* from 0 *)
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
	1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; (* to 127 *)
	0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
	0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
	0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
	0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
	0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; (* from 194 to 223 *)
	2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
	3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; (* from 224 to 239 *)
	4; 4; 4; 4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0 |] (* from 240 to 247 *)

let uchar_utf8 i =
  let b0 = i () in
  begin match utf8_len.(b0) with
  | 0 -> raise Malformed
  | 1 -> b0
  | 2 ->
      let b1 = i () in
      if b1 lsr 6 != 0b10 then raise Malformed else
      ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)
  | 3 ->
      let b1 = i () in
      let b2 = i () in
      if b2 lsr 6 != 0b10 then raise Malformed else
      begin match b0 with
      | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then raise Malformed else ()
      | 0xED -> if b1 < 0x80 || 0x9F < b1 then raise Malformed else ()
      | _ -> if b1 lsr 6 != 0b10 then raise Malformed else ()
      end;
      ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
  | 4 -> 
      let b1 = i () in
      let b2 = i () in
      let b3 = i () in
      if  b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then raise Malformed else
      begin match b0 with
      | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then raise Malformed else ()
      | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then raise Malformed else ()
      | _ -> if b1 lsr 6 != 0b10 then raise Malformed else ()
      end;
      ((b0 land 0x07) lsl 18) lor ((b1 land 0x3F) lsl 12) lor 
      ((b2 land 0x3F) lsl 6) lor (b3 land 0x3F)
  | _ -> assert false
  end

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

(*****************************************************************************)
(* the buffer will be used by every output. *)
let b = Buffer.create 16384
let outc c = Buffer.add_char b c
let outs s = Buffer.add_string b s
let num_sp = 2

let ppSingleQuotes str =
	outc '\'';
	outs str;
	outc '\''

let ppDoubleQuotes str =
	outc '"';
	outs str;
	outc '"'

let ppNoQuotes str = outs str

(** [mustUseSingle uchar] is true whenever it would not be possible to print 
a string containing [uchar] using the plain style, but it would be possible
with single quote style. Rules enforced are no ": " sequence, no " #" sequence,
no ',', no '[', no ']', no '{', and no '}'. *)
let mustUseSingle =
	(* last char read, initially 0 *)
	let lastChar = ref 0 in
	function uchar ->
		let res =
			(* Plain scalars must never contain the ": " and " #" character combinations *)
			!lastChar = 0x3A (* ':' *) && uchar = 0x20 ||
			!lastChar = 0x20 && uchar = 0x23 (* '#' *) ||
			(* In addition, inside flow collections, plain scalars must not contain *)
			(* the '[', ']', '{', '}' and ',' characters. *)
			(* Note: we further restrict plain scalars by applying this regardless of *)
			(* whether the scalar is in a flow collection. *)
			uchar = 0x2C (* ',' *) ||
			uchar = 0x5B || uchar = 0x5D || (* '[', ']' *)
			uchar = 0x7B || uchar = 0x7D (* '{', '}' *)
		in
		lastChar := uchar;
		res

(** [mustUseDouble uchar] is true whenever it would not be possible to print 
a string containing [uchar] using the plain style nor the single quote style,
but it would be possible with double quote style.
Rules enforced are no non-printable characters in the string (as required by
the standard), as well as no line breaks. *)
let mustUseDouble uchar =
	(* C0 block, except TAB 0x09 *)
	(* We consider that LF 0x0A and CR 0x0D require double-quoting style, for *)
	(* the sake of simplicity. *)
	uchar >= 0 && uchar < 0x09 || uchar > 0x09 && uchar < 0x20 ||
	
	(* DEL, C1 block, except NEL 0x85 *)
	uchar >= 0x7F && uchar <= 0x9F && uchar <> 0x85 ||
	
	(* surrogate block 0xD800-0xDFFF, and 0xFFFE and 0xFFFF *)
	uchar >= 0xD800 && uchar <= 0xDFFF || uchar = 0xFFFE || uchar = 0xFFFF

exception Done

(* this function prints the given string using double-quoted, *)
(* single-quoted, or plain style according to the characters *)
(* the string contains. *)
let ppString str =
	let i = ref 0 in
	let length = String.length str in
	
	(* read_char is used by uchar_utf8 to read UTF-8 bytes from [str] and *)
	(* return a Unicode char (as an int). *)
	let read_char () =
		if !i = length then
			raise Malformed
		else
			let char = String.unsafe_get str !i in
			incr i;
			int_of_char char
	in

	try
		(* a plain scalar must not contain leading or trailing white space characters. *)
		
		
		(* for each Unicode char in str, check whether it would be necessary to *)
		(* use single quote style or double quote style. *)
		(* Note that we do not use String.iter because we deal with UTF-8 here. *)
		while !i < length do
			let uchar = uchar_utf8 read_char in
			if mustUseSingle uchar then (
				ppSingleQuotes str;
				raise Done
			);

			if mustUseDouble uchar then (
				ppDoubleQuotes str;
				raise Done
			)
		done;
		
		if length = 0 then (
			(* a plain scalar must not be empty *)
			ppSingleQuotes str
		) else if length = 1 then (
			i := 0;
			let s0 = uchar_utf8 read_char in
			match s0 with
				| 0x2D | 0x3A | 0x3F -> ppSingleQuotes str
				| _ -> ppNoQuotes str
		) else (
			(* check whether the two first chars allow the use of plain style *)

			(* the ':', '?' and '-' indicators may be used as the first character *)
			(* if followed by a non-space character, as this causes no ambiguity. *)
			i := 0;
			let s0 = uchar_utf8 read_char in
			let s1 = uchar_utf8 read_char in
			match (s0, s1) with
				| (0x2D, 0x09) | (0x2D, 0x20) (* '-' = 0x2D *)
				| (0x3A, 0x09) | (0x3A, 0x20) (* ':' = 0x3A *)
				| (0x3F, 0x09) | (0x3F, 0x20) (* '?' = 0x3F *) -> ppSingleQuotes str
				| _ ->
					(* nothing prevents us from writing a plain style scalar *)
					ppNoQuotes str
		)
	with Done -> ()

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
