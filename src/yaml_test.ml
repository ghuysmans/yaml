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
	let node1 =
		mkSeqFlow
			[mkBool false; mkInt 1;
			mkMapFlow [ (mkStr "p1", mkStr "p2"); (mkStr "p3", mkStr "p4") ];
			mkInt 2 ]
	in
	let node2 = mkStr "toto" in
	
	let node4 = mkStr "tata" in
	let node5 = mkFloat 3.5 in
	let node6 = mkInt 56 in
	let node7 = mkNull () in
	let node8 =
		mkSeqBlock
			[mkBool false; mkInt 1; mkSeqBlock [mkStr "a"; mkStr "b"]; mkInt 2;
			mkSeqFlow [mkStr "c"; mkStr "d"]]
	in
	let node3 = mkMapBlock [(node4, node5); (node5, node8); (node6, node7)] in
	let node = mkSeqBlock [node1; node2; node3] in
	let doc = mkDoc node in
	
	let oc = open_out "yaml_test.txt" in
	ppDoc oc doc;
	close_out oc
