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

module SH : Hashtbl.S with type key = string

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

val mkBinary : string -> node

val mkBool : bool -> node

val mkFloat : float -> node

val mkInt : int -> node

val mkNull : unit -> node

val mkStr : string -> node

val mkMap : ?style:style -> (node * node) list -> node

val mkMapBlock : (node * node) list -> node

val mkMapFlow : (node * node) list -> node

val mkSeq : ?style:style -> node list -> node

val mkSeqBlock : node list -> node

val mkSeqFlow : node list -> node

val mkDoc : node -> doc

val isMap : node -> bool

val isScalar : node -> bool

val isSeq : node -> bool

val ppDoc : Format.formatter -> doc -> unit
