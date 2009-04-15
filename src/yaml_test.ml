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

open Yaml

let _ =
	let unquoted =
		mkSeq
			[ mkStr "this does not need quotes" ]
	in

	let single =
		mkSeq
			[mkStr "a{b"; mkStr "x}y"; mkStr "[z"; mkStr "c]";
			mkStr "t,u"; mkStr "a: b"; mkStr "a #b"; mkStr "-";
			mkStr "a:b"; mkStr "a#b"; mkStr "a\\b"; mkStr "\"xyz\"";
			mkStr "here's to \"quotes\""; mkStr "escaping? \\"]
	in

	(* UTF-8 tests. *)
	(* First word is "mosquito" in Romanian. *)
	(* Second word is "Naruto" in Japanese hiragana. *)
	let utf8_str =
		"\r\n" ^ utf8 [| 0x0163; 0x103; 0x6E; 0x0163; 0x61; 0x72|] ^
		" " ^ utf8 [| 0x306A; 0x308B; 0x3068 |]
	in
	let double = mkSeq [mkStr utf8_str ] in

	let strings =
		mkMap
			[ (mkStr "unquoted", unquoted);
			(mkStr "single quoted", single);
			(mkStr "double quoted", double) ]
	in
	
	let others =
		mkSeq
			[ mkBool false; mkBool true; mkFloat 3.5; mkInt 45; mkNull () ]
	in
	
	let node =
		mkMap
			[ (mkStr "string literals", strings);
			(mkStr "other literals", others) ]
	in
	let doc = mkDoc node in
	
	let oc = open_out "yaml_test.txt" in
	ppDoc oc doc;
	close_out oc
