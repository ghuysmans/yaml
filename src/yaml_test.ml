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
	let src = mkStr ~anchor:"a1" "node with an anchor" in
	let unquoted =
		mkSeq
			[ mkStr "a b"; mkStr "a#b"; mkStr "a\\b"; mkStr "here's to \"quotes\"";
			src ]
	in

	let single =
		mkSeq
			[mkSeq
				[mkStr "a:b"; mkStr "a{b"; mkStr "x}y";
				mkStr "[z"; mkStr "c]"; mkStr "t,u"; mkStr "a: b";
				mkStr "a #b"; mkStr "-"];
			mkSeq
				[mkStr "http://example.com/foo#bar";
				mkStr "\"xyz\""; mkStr "???"; mkStr " spaces1"; mkStr "spaces2 "];
			mkSeq
				[mkStr "\"here's to 'quotes'\""; mkStr ("NEL char:" ^ utf8 [| 0x85 |]) ]
			]
	in

	(* UTF-8 tests. *)
	(* First word is "mosquito" in Romanian. *)
	(* Second word is "Naruto" in Japanese hiragana. *)
	let utf8_str =
		"\r\n" ^ utf8 [| 0x2028; 0x2029; 0x0163; 0x103; 0x6E; 0x0163; 0x61; 0x72|] ^
		" " ^ utf8 [| 0x306A; 0x308B; 0x3068 |]
	in
	let double = mkSeq [mkStr utf8_str ] in

	let strings =
		mkMap
			[ (mkStr "unquoted", unquoted);
			(mkStr "single quoted", single);
			(mkStr "double quoted", double);
			(mkStr "aliases", mkAlias src) ]
	in
	
	let others =
		mkSeq
			[ mkBool false; mkBool true; mkFloat 3.5; mkInt 45; mkNull () ]
	in
	
	
  let map =
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
	
	let node =
		mkMap
			[ (mkStr "string literals", strings);
			(mkStr "other literals", others);
			(mkStr "a map", map) ]
	in
	let doc = mkDoc node in
	
	let oc = open_out "yaml_test.txt" in
	dump oc doc;
	close_out oc
