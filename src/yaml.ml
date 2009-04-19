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

type anchor = string

type scalar =
	| Binary of string
	| Bool of bool
	| Float of float
	| Int of int
	| Str of string

type style =
	| Block
	| Flow

type kind =
	| Alias of node
	| Map of (node * node) list
	| Null
	| Scalar of scalar
	| Seq of node list

and node = {
	anchor : anchor option;
	mutable kind : kind;
	mutable tag : tag option;
}

type doc = {
	node : node;
	tags : tag SH.t;
}

let mkNode ?anchor kind =	{
		anchor = anchor;
		kind = kind;
		tag = None;
	}

let mkAlias ?anchor anchorSrc = mkNode ?anchor (Alias anchorSrc)

let mkBinary ?anchor b = mkNode ?anchor (Scalar (Binary b))

let mkBool ?anchor b = mkNode ?anchor (Scalar (Bool b))

let mkFloat ?anchor f = mkNode ?anchor (Scalar (Float f))

let mkInt ?anchor i = mkNode ?anchor (Scalar (Int i))

let mkStr ?anchor s = mkNode ?anchor (Scalar (Str s))

let mkMap ?anchor map = mkNode ?anchor (Map map)

let mkNull ?anchor () = mkNode ?anchor Null

let mkSeq ?anchor list = mkNode ?anchor (Seq list)

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
	String.iter
		(fun char ->
			match char with
				| '\'' ->
					outc '\'';
					outc '\''
				| _ ->
					outc char)
	str;
	outc '\''

let ppDoubleQuotes str =
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
	
	(* for each Unicode char in str, check whether it is necessary to escape *)
	outc '"';
	while !i < length do
		let uchar = uchar_utf8 read_char in
		match uchar with
			| 0 -> outs "\\0"
			| 0x07 -> outs "\\a" (* bell *)
			| 0x08 -> outs "\\b" (* backspace *)
			| 0x09 -> outs "\\t" (* tab *)
			| 0x0A -> outs "\\n" (* line feed *)
			| 0x0B -> outs "\\v" (* vertical tab *)
			| 0x0D -> outs "\\r" (* carriage return *)
			| 0x1B -> outs "\\e" (* espace *)
			| 0x22 -> outs "\\\"" (* double quote *)
			| 0x5C -> outs "\\\\" (* back slash *)
			| 0x85 -> outs "\\N" (* next line *)
			| 0xA0 -> outs "\\_" (* Unicode non-breaking space *)
			| 0x2028 -> outs "\\L" (* Unicode line separator *)
			| 0x2029 -> outs "\\P" (* Unicode paragraph separator *)

			| _ when uchar < 0x20 -> outs (sprintf "\\x%x" uchar)

			| 0xFFFE | 0xFFFF
			| _ when uchar >= 0xD800 && uchar <= 0xDFFF ->
				outs (sprintf "\\u%x" uchar)

			(* all other chars are printable *)
			| _ -> outs (utf8 [| uchar |])

	done;
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
			!lastChar = 0x20 && uchar = 0x23 (* '#' *) ||
			
			match uchar with
			(* Note: It appears that some YAML implementations don't cope particularly *)
			(* well with ':' or '?' characters even when they are not surrounded by *)
			(* spaces, so we forbid them in plain scalars too. *)
			| 0x3A (* ':' *) | 0x3F (* '?' *)

			(* In addition, inside flow collections, plain scalars must not contain *)
			(* the '[', ']', '{', '}' and ',' characters. *)
			(* Note: We further restrict plain scalars by applying this regardless of *)
			(* whether the scalar is in a flow collection. *)
			| 0x2C (* ',' *)
			| 0x5B (* '[' *) | 0x5D (* ']' *)
			| 0x7B (* '{' *) | 0x7D (* '}' *) -> true

			| _ -> false
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

	if length = 0 then (
		(* a plain scalar must not be empty *)
		ppSingleQuotes str
	) else (
		(* a plain scalar must not contain leading or trailing white space characters. *)
		let c0 = int_of_char (String.unsafe_get str 0) in
		let cl = int_of_char (String.unsafe_get str (length - 1)) in
		if c0 = 0x09 || c0 = 0x20 || cl = 0x09 || cl = 0x20 ||
			(* Additional restriction: a plain scalar must not begin with *)
			(* '"' = 0x22 or '\'' = 0x27 *)
			c0 = 0x22 || c0 = 0x27 then
			ppSingleQuotes str
		else
			try
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
	
				if length = 1 then (
					i := 0;
					let s0 = uchar_utf8 read_char in
					match s0 with
						(* '-', ':', '?' *)
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
	)

let ppScalar = function
	| Binary bin -> outc '"'; outs bin; outc '"'
	| Bool bool -> outs (string_of_bool bool)
	| Float f -> outs (string_of_float f)
	| Int i -> outs (string_of_int i)
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

(** [checkAnchor anchor] checks that the anchor name is valid according to
YAML 1.2 spec *)
let checkAnchor anchor =
	if anchor = "" then
		invalid_arg "An anchor name may not be empty";

	(* TODO: also check for non-printable characters *)

	(* Anchor names must not contain the '[', ']', '{', '}' and ',' characters. *)
	String.iter
		(fun char ->
			match int_of_char char with
			| 0x09 | 0x0A | 0x0D | 0x20 ->
				invalid_arg "Anchor names must not contain white space or line break characters." 

			| 0x2C (* ',' *)
			| 0x5B (* '[' *) | 0x5D (* ']' *)
			| 0x7B (* '{' *) | 0x7D (* '}' *) ->
				invalid_arg "Anchor names must not contain the '[', ']', '{', '}' and ',' characters."
			| _ -> ())
	anchor

let ppAlias node =
	match node.anchor with
		| None -> assert false (* checked by mkAlias *)
		| Some anchor ->
			checkAnchor anchor;
			outs "*"; outs anchor

let rec ppNode indent node =
	(match node.anchor with
		| None -> ()
		| Some anchor ->
			checkAnchor anchor;
			outc '&';
			outs anchor;
			outc ' ');
	
	match node.kind with
		| Alias node -> ppAlias node 
		| Map map -> ppMap indent (mapStyle map) map
		| Null -> outs "null"
		| Scalar scalar -> ppScalar scalar
		| Seq list -> ppSeq indent (seqStyle list) list

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

let dump oc doc =
	Buffer.clear b;
	outs "\xEF\xBB\xBF"; (* UTF-8 BOM *)
	outs "%YAML 1.2\n---\n"; (* YAML 1.2 declaration *)
	ppNode 0 doc.node;
	Buffer.output_buffer oc b;
	flush oc
