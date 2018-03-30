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
open Expressions

let args = []
let reset_args () = ()

let pc_err src loc = 
  let msg = 
    match src with
    | `Pat ->
      "Expecting a singleton variable pattern!"
    | `Exp ->
      "Expecting a single constant expression!"
    | `Binding ->
      "Expecting a single value or binding!" in
  Location.raise_errorf ~loc "%s" msg

let dest_exp_const =
  function
  | { pexp_desc = Pexp_constant Pconst_float   (c, _) }
  | { pexp_desc = Pexp_constant Pconst_string  (c, _) }
  | { pexp_desc = Pexp_constant Pconst_integer (c, _) } ->
    c
  | { pexp_loc } ->
    pc_err `Exp pexp_loc

let dest_pc_ext =
  function
  | { pexp_desc = Pexp_extension ({ txt }, payload); pexp_loc } 
      when is_pc_ext txt ->
    begin match payload with
    | PStr [] ->
      Some (None, None)
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (_, bindings, cont) }, 
        _)}] ->
      let bindings =
        List.map
          (function
            | { pvb_pat = { ppat_desc = Ppat_var _ }; 
                pvb_expr } as pvb ->
              Some pvb, (dest_exp_const pvb_expr, pvb_expr.pexp_loc)
            | { pvb_loc } ->
              pc_err `Pat pvb_loc)
          (bindings) in
      Some (Some bindings, Some cont)
    | PStr [{ pstr_desc = Pstr_eval (({ pexp_loc } as e), _) } ] ->
      Some (Some [(None, (dest_exp_const e, pexp_loc))], None)
    | _ ->
      pc_err `Binding pexp_loc
    end
  | { pexp_desc = Pexp_extension ({ txt }, _) } ->
    let () = prerr_endline txt in None
  | _ ->
    None

let dest_lift_ext =
  function
  | { pexp_desc = Pexp_extension ({ txt }, payload); pexp_loc } 
      when is_lift_ext txt ->
    failwith "Not implemented yet!"
  | _ ->
    None

let dest_model_ext =
  function
  | { pexp_desc = Pexp_extension ({ txt }, payload); pexp_loc } 
      when is_model_ext txt ->
    failwith "Not implemented yet!"
  | _ ->
    None
    
let dest_ext expr =
  match dest_pc_ext expr    with Some res ->
    `PC res | _ ->
  match dest_lift_ext expr  with Some res ->
    `LIFT res | _ ->
  match dest_model_ext expr with Some res ->
    `MODEL res 
  | _ ->
    `NOEXT

let mk_pc ?init loc =
  let loc = { loc with loc_ghost = true} in
  let args = [ (Nolabel, unit) ] in
  let args = 
    match init with
    | None ->
      args
    | Some c ->
      (Labelled ("init"), Exp.constant (Const.string c)) :: args in
  Exp.apply ~loc (mk_ident Model.pc) args

let rewriter _ _ =
  { default_mapper with
    expr = fun mapper expr ->
      match dest_ext expr with
      | `PC (None, None) ->
        mk_pc expr.pexp_loc
      | `PC (Some [(None, (init, loc))], None) ->
        mk_pc ~init loc
      | `PC (Some bindings, cont) ->
        let bindings = 
          List.map 
            (fun (vb, (init, loc)) -> 
              let vb = Option.get_exn vb in
              { vb with pvb_expr = mk_pc ~init loc }) 
            (bindings) in
        let cont = mapper.expr mapper (Option.get_exn cont) in
        Exp.let_ Nonrecursive bindings cont
      | _ -> 
        default_mapper.expr mapper expr ; }

let () =
  Driver.register ~name:"decml" ~reset_args ~args
    Versions.ocaml_404 rewriter