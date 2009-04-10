open Ocamlbuild_plugin
open Command

let _ =
  dispatch
    (function
     | After_rules ->
         (* Documentation: colorize code. *)
         flag [ "doc" ]
           (S
              [ A "-colorize-code"; A "-t"; A "Yaml documentation" ])
     | _ -> ())
