open Migrate_parsetree
open   Ast_404

let args = []
let reset_args () = ()

let rewriter _ _ =
  Ast_mapper.default_mapper

let () =
  Driver.register ~name:"decml" ~reset_args ~args
    Versions.ocaml_404 rewriter