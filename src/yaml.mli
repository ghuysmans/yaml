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
dump oc doc;
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

(** {6 Node properties} *)

type tag = {
	mutable shorthand : string; (** The tag shorthand. *)
	mutable full : string; (** The tag full name. *)
	mutable verbatim : bool; (** Whether the tag is verbatim. *)
}
(** A YAML tag. Not implemented yet. *)

type anchor = string
(** A YAML anchor. *)

(** {6 Nodes} *)

type scalar =
	| Binary of string (** A YAML binary string. Will be encoded in base64. *)
	| Bool of bool (** A YAML boolean. *)
	| Float of float (** A YAML floating point number. *)
	| Int of int (** A YAML integer. *)
	| Str of string (** A YAML UTF-8 encoded string. *)
(** A YAML scalar. *) 

type kind =
	| Alias of node (** A YAML alias node. *)
	| Map of (node * node) list (** A YAML map of {!Yaml.node}s as an association list. *)
	| Null (** A YAML empty node. *)
	| Scalar of scalar (** A {!Yaml.scalar} node. *)
	| Seq of node list (** A YAML sequence of {!Yaml.node}s. *)
(** Different kinds of YAML nodes. *)

and node = {
	anchor : anchor option;
(** the node's optional anchor. This field is persistent because nodes may
reference other nodes by their anchor. *)
	mutable kind : kind; (** the node kind. *)
	mutable tag : tag option; (** the node's optional tag. *)
}
(** A YAML node. Has a {!Yaml.kind} and a possibly empty {!Yaml.tag}. *)

(** {7 Node creation functions}

The following functions allow the creation of different YAML nodes. Each of
them take a first [?anchor:anchor] parameter, that is a optional anchor.
Once a node is created, it is not possible to modify its anchor.
This ensures that alias nodes referencing it do not become invalid.
The anchor name is still mutable though as it is just a string.
As a consequence, the validity of an anchor name is only checked when
{!Yaml.dump}ing a document.
An empty anchor, or an anchor with ['\['], ['\]'], ['\{'], ['\}'] or [',']
characters will result in an [Invalid_argument] exception being raised.
*)

val mkAlias : ?anchor:anchor -> node -> node
(** [mkAlias ?anchor node] returns an untagged {!Yaml.node} whose kind is
[Alias node]. *)

val mkBinary : ?anchor:anchor -> string -> node
(** [mkBinary ?anchor string] returns an untagged {!Yaml.node} whose kind is
[Scalar (Binary str)]. *) 

val mkBool : ?anchor:anchor -> bool -> node
(** [mkBool ?anchor bool] returns an untagged {!Yaml.node} whose kind is
[Scalar (Bool bool)]. *)

val mkFloat : ?anchor:anchor -> float -> node
(** [mkFloat ?anchor float] returns an untagged {!Yaml.node} whose kind is
[Scalar (Float float)]. *)

val mkInt : ?anchor:anchor -> int -> node
(** [mkInt ?anchor int] returns an untagged {!Yaml.node} whose kind is
[Scalar (Int int)]. *)

val mkStr : ?anchor:anchor -> string -> node
(** [mkStr ?anchor str] returns an untagged {!Yaml.node} whose kind is
[Scalar (Str str)]. *)

val mkMap : ?anchor:anchor -> (node * node) list -> node
(** [mkMap ?anchor map] returns an untagged {!Yaml.node} whose kind is
[Map map]. *)

val mkNull : ?anchor:anchor -> unit -> node
(** [mkNull ?anchor ()] returns an untagged {!Yaml.node} whose kind is
[Null]. *)

val mkSeq : ?anchor:anchor -> node list -> node
(** [mkSeq ?anchor list] returns an untagged {!Yaml.node} whose kind is
[Seq list]. *)

(** {7 Node test functions} *)

val isCollection : node -> bool
(** [isCollection node] returns [true] if [node] is a [Yaml.Map] or
[Yaml.Seq], and [false] otherwise. *)

val isMap : node -> bool
(** [isMap node] returns [true] if [node] is a [Yaml.Map], and [false]
otherwise. *)

val isScalar : node -> bool
(** [isScalar node] returns [true] if [node] is a [Yaml.Scalar], and [false]
otherwise. *)

val isSeq : node -> bool
(** [isSeq node] returns [true] if [node] is a [Yaml.Seq], and [false]
otherwise. *)

(** {6 Encoding functions} *)

val utf8 : int array -> string
(** [utf8 chars] takes a Unicode character array, translates each of them to
UTF-8 bytes, and returns a string that contains the resulting bytes. *)

(** {6 YAML Document} *)

type doc
(** The type of a document. The current implementation allows a document to contain only
a single stream. A stream contains a single top-level node. A document is pretty-printed
to YAML by calling {!Yaml.dump}. *)

val mkDoc : node -> doc
(** [mkDoc node] creates a new {!Yaml.doc} that contains a single top-level node. *)

val dump : out_channel -> doc -> unit
(** [dump out doc] pretty-prints the given {!Yaml.doc} document as YAML.

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
      {- with single quotes if it does not contain non-printable characters, or}
      {- with double quotes}
    }
  }
}

*)
