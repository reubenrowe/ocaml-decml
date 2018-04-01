open Containers

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

(* TODO: Make sure that locations are set properly (also with the ghost flag 
         set) so that PPX reports errors matched with the relevant places in the
         source code. *)

let transform tx e =
  Exp.apply (mk_ident Model.tx) [ (Nolabel, tx); (Nolabel, e)]
  
let weaken n ({ pexp_loc } as e) =
  let pexp_loc = 
    if n = 0 then pexp_loc else { pexp_loc with loc_ghost = true } in
  let txs = List.replicate n (Exp.construct (mknoloc Model._Weak) None) in
  let e = List.fold_right transform txs e in
  { e with pexp_loc }

let var idx loc =
  let loc = { loc with loc_ghost = true } in
  let txs = 
    List.replicate idx 
      (Exp.construct (mknoloc Model._Cong)
        (Some (Exp.construct (mknoloc Model._Weak) None))) in
  let var = mk_ident Model.var in
  let e = List.fold_right transform txs var in
  { e with pexp_loc = loc }
  
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

let abs body pexp_loc =
  let pexp_loc = { pexp_loc with loc_ghost = true } in
  let e = Exp.apply (mk_ident Model.abs) [Nolabel, body] in
  { e with pexp_loc }

let apply ?(lbl=Nolabel) e e' =
  let app = mk_ident Model.app in
  Exp.apply app [(Nolabel, e); (lbl, e')]
