(*****************************************************************************)
(* Yaml                                                                      *)
(* Copyright (c) 2009, IETR/INSA of Rennes.                                  *)
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

(** YAML parser and pretty-printer. 

{ul
{- The parser has not been implemented yet.}
{- Emitting YAML:
{[
let node =
  Yaml.mkMap
    [
      (Yaml.mkStr "american",
       Yaml.mkSeq
         [Yaml.mkStr "Boston Red Sox";
          Yaml.mkStr "Detroit Tigers";
          Yaml.mkStr "New York Yankees"]);

      (Yaml.mkStr "national",
       Yaml.mkSeq
         [Yaml.mkStr "New York Mets";
          Yaml.mkStr "Chicago Cubs";
          Yaml.mkStr "Atlanta Braves"])
    ]
in

let doc = mkDoc node in
let oc = open_out "output.txt" in
ppDoc oc doc;
close_out oc
]}

"output.txt" will contain something very similar to:
{v %YAML 1.2
---
american: [Boston Red Sox, Detroit Tigers, New York Yankees]
national: [New York Mets, Chicago Cubs, Atlanta Braves] v}}}

See {{:http://yaml.org/spec/1.2/} YAML 1.2} for more information on the
YAML 1.2 specification.

@author Matthieu Wipliez
*)

(** {6 Tags} *)

type tag = {
	mutable shorthand : string; (** The tag shorthand. *)
	mutable full : string; (** The tag full name. *)
	mutable verbatim : bool; (** Whether the tag is verbatim. *)
}
(** A YAML tag. Not implemented yet. *)

(** {6 Nodes} *)

type scalar =
	| Binary of string (** A YAML binary string. Will be encoded in base64. *)
	| Bool of bool (** A YAML boolean. *)
	| Float of float (** A YAML floating point number. *)
	| Int of int (** A YAML integer. *)
	| Null (** A YAML null value. *)
	| Str of string (** A YAML string. *)
(** A YAML scalar. *) 

type kind =
	| Scalar of scalar (** A {Yaml.scalar} node. *)
	| Seq of node list (** A YAML sequence of {!Yaml.node}s. *)
	| Map of (node * node) list (** A YAML map of {!Yaml.node}s as an association list. *)
(** Different kinds of YAML nodes. *)

and node = {
	kind : kind;
	mutable tag : tag option;
}
(** A YAML node. Has a {!Yaml.kind} and a possibly empty {!Yaml.tag}. *)

(** {7 Node functions} *)

val mkBinary : string -> node
(** [mkBinary string] returns an untagged {!Yaml.node} whose kind is
[Scalar (Binary str)]. *) 

val mkBool : bool -> node
(** [mkBool bool] returns an untagged {!Yaml.node} whose kind is
[Scalar (Bool bool)]. *)

val mkFloat : float -> node
(** [mkFloat float] returns an untagged {!Yaml.node} whose kind is
[Scalar (Float float)]. *)

val mkInt : int -> node
(** [mkInt int] returns an untagged {!Yaml.node} whose kind is
[Scalar (Int int)]. *)

val mkNull : unit -> node
(** [mkNull ()] returns an untagged {!Yaml.node} whose kind is
[Scalar Null]. *)

val mkStr : string -> node
(** [mkStr str] returns an untagged {!Yaml.node} whose kind is
[Scalar (Str str)]. *)

val mkMap : (node * node) list -> node
(** [mkMap map] returns an untagged {!Yaml.node} whose kind is
[Map map]. *)

val mkSeq : node list -> node
(** [mkSeq list] returns an untagged {!Yaml.node} whose kind is
[Seq list]. *)

val isMap : node -> bool

val isScalar : node -> bool

val isSeq : node -> bool

(** {7 Encoding functions} *)

val utf8 : int array -> string
(** [utf8 chars] takes a Unicode character array, translates each of them to
UTF-8 bytes, and returns a string that contains the resulting bytes. *)

(** {6 YAML Document} *)

type doc
(** The type of a document. The current implementation allows a document to contain only
a single stream. A stream contains a single top-level node. A document is pretty-printed
to YAML by calling {!Yaml.ppDoc}. *)

val mkDoc : node -> doc
(** [mkDoc node] creates a new [Yaml.doc] that contains a single top-level node. *)

val ppDoc : out_channel -> doc -> unit
(** [ppDoc out doc] pretty-prints the given [Yaml.doc] document as YAML. 

A few notes:
{ul
  {- the pretty-printer follows the established principle in other YAML implementations
concerning which sequences and maps have flow or block styles. Namely, a sequence/map
has flow style if it contains only scalars; it has block style otherwise.}
  {- boolean are printed as [true] and [false].}
  {- floats and integers are printed without quotes.}
  {- strings are printed as follows:
    {ul
      {- without any quotes if it does not contain any characters that need to be espaced, or}
      {- with single quotes if it contains ['"'] or [\ ], or}
      {- with double quotes}
    }
  }
}

*)
