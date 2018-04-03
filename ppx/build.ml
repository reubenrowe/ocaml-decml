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
  
let weaken n e pexp_loc =
  let pexp_loc = { pexp_loc with loc_ghost = true } in
  let txs = List.replicate n (Exp.construct (mknoloc Model._Weak) None) in
  let e = List.fold_right transform txs e in
  { e with pexp_loc }

let var idx scope_depth pexp_loc =
  assert (idx < scope_depth) ;
  let pexp_loc = { pexp_loc with loc_ghost = true } in
  let txs = 
    List.replicate (scope_depth - (idx + 1)) 
      (Exp.construct (mknoloc Model._Cong)
        (Some (Exp.construct (mknoloc Model._Weak) None))) in
  let var = mk_ident Model.var in
  let e = List.fold_right transform txs var in
  let txs = List.replicate idx (Exp.construct (mknoloc Model._Weak) None) in
  let e = List.fold_right transform txs e in
  { e with pexp_loc }
  
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

let lift e scope_depth loc =
  let loc = { loc with loc_ghost = true } in
  let e =
    Exp.construct
      (mknoloc Model._Ex)
      (Some 
        (Exp.tuple [
            Exp.construct (mknoloc Model._Lifted) (Some e) ;
            mk_ident Model.Parameters.null ;
          ])) in
  weaken scope_depth e loc

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

let abs_rec scope_depth body pexp_loc =
  let pexp_loc = { pexp_loc with loc_ghost = true } in
  let body = 
    apply 
      (weaken 1 body Location.none)
      (var scope_depth (scope_depth + 2) Location.none) in
  let e = Exp.apply (mk_ident Model.abs_rec) [Nolabel, body] in
  { e with pexp_loc }

let let_bind bindings cont pexp_loc =
  let pexp_loc = { pexp_loc with loc_ghost = true } in
  let let_bind e cont =
    Exp.apply (mk_ident Model.let_bind) [ (Nolabel, e) ; (Nolabel, cont) ] in
  let e = List.fold_right let_bind bindings cont in
  { e with pexp_loc }

let letrec_bind scope_depth f cont pexp_loc =
  let pexp_loc = { pexp_loc with loc_ghost = true } in
  let f = abs_rec scope_depth f Location.none in
  let e = 
    Exp.apply (mk_ident Model.let_bind) [(Nolabel, f) ; (Nolabel, cont)] in
  { e with pexp_loc }

let pair e es loc =
  match es with
  | [ _; _ ] ->
    let loc = { loc with loc_ghost = true } in
    Exp.apply ~loc
      (mk_ident Model.pair)
      [ (Nolabel, { e with pexp_desc = Pexp_tuple es }) ]
  | _ ->
    invalid_arg (Format.sprintf "%s.pair" __MODULE__)

let ifthenelse (_test, _then, _else) loc =
  let loc = { loc with loc_ghost = true } in
  Exp.apply ~loc
    (mk_ident Model.ifelse)
    [ (Nolabel, _test) ; (Nolabel, _then) ; (Nolabel, _else) ;]