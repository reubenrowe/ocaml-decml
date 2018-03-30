open Migrate_parsetree
open   Ast_404
open     Ast_helper
open     Ast_mapper
open     Parsetree
open   Compiler_libs
open     Asttypes
open     Location
open     Longident

open Identifiers

let unit =
  Exp.construct (mknoloc unit) None

let prov_const ?init loc =
  let loc = { loc with loc_ghost = true } in
  let args = [ (Nolabel, unit) ] in
  let args = 
    match init with
    | None ->
      args
    | Some c ->
      (Labelled ("init"), Exp.constant (Const.string c)) :: args in
  Exp.apply ~loc (mk_ident Model.pc) args

let lift e loc =
  let loc = { loc with loc_ghost = true } in
  Exp.construct ~loc
    (mknoloc Model._Ex)
    (Some 
      (Exp.tuple [
          Exp.construct (mknoloc Model._Lifted) (Some e) ;
          mk_ident Model.Parameters.null ;
        ]))

let decouple ({ pvb_pat } as pvb) e =
  let { ppat_loc = loc } = pvb_pat in
  let pvb_pat = Pat.construct ~loc (mknoloc Model._Ex) (Some pvb_pat) in
  { pvb with pvb_pat ; pvb_expr = e }