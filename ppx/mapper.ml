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

open Build
open Identifiers

let args = []
let reset_args () = ()

let dest_exp_const =
  function
  | { pexp_desc = Pexp_constant Pconst_float   (c, _) }
  | { pexp_desc = Pexp_constant Pconst_string  (c, _) }
  | { pexp_desc = Pexp_constant Pconst_integer (c, _) } ->
    c
  | { pexp_loc } ->
    Err.const_exp pexp_loc

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
            | { pvb_pat = { ppat_desc = Ppat_var _ }; pvb_expr } as pvb ->
              Some pvb, (dest_exp_const pvb_expr, pvb_expr.pexp_loc)
            | { pvb_loc } ->
              Err.pattern pvb_loc)
          (bindings) in
      Some (Some bindings, Some cont)
    | PStr [{ pstr_desc = Pstr_eval (({ pexp_loc } as e), _) } ] ->
      Some (Some [(None, (dest_exp_const e, pexp_loc))], None)
    | _ ->
      Err.payload pexp_loc
    end
  | _ ->
    None

let dest_lift_ext =
  function
  | { pexp_desc = Pexp_extension ({ txt }, payload); pexp_loc } 
      when is_lift_ext txt ->
    begin match payload with
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (_, bindings, cont) }, 
        _)}] ->
      let bindings =
        List.map
          (function
            | { pvb_pat = { ppat_desc = Ppat_var _ }; pvb_loc } as pvb ->
              Some pvb, (None, pvb_loc)
            | { pvb_loc } ->
              Err.pattern pvb_loc)
          (bindings) in
      Some (bindings, Some cont)
    | PStr [{ pstr_desc = Pstr_eval (({ pexp_loc } as e), _) } ] ->
      Some ([(None, (Some e, pexp_loc))], None)
    | _ ->
      Err.payload pexp_loc
    end
  | _ ->
    None

let dest_model_ext =
  function
  | { pexp_desc = Pexp_extension ({ txt }, payload); pexp_loc } 
      when is_model_ext txt ->
    failwith "Not implemented yet!"
  | _ ->
    None

let dest_decouple_ext =
  function
  | { pexp_desc = Pexp_extension ({ txt }, payload); pexp_loc } 
      when is_decouple_ext txt ->
    begin match payload with
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (_, bindings, cont) }, 
        _)}] ->
      let () =
        List.iter
          (function
            | { pvb_pat = { 
                  ppat_desc = Ppat_tuple [
                    { ppat_desc = Ppat_var _ } ; 
                    { ppat_desc = Ppat_var _ } ; ] }; 
                pvb_expr; pvb_loc } ->
              ()
            | { pvb_loc } ->
              Err.decouple pvb_loc)
          (bindings) in
      Some (bindings, cont)
    | _ ->
      Err.payload pexp_loc
    end
  | _ ->
    None
        
let dest_ext expr =
  match dest_pc_ext expr       with Some res ->
    `PC res | _ ->
  match dest_lift_ext expr     with Some res ->
    `LIFT res | _ ->
  match dest_model_ext expr    with Some res ->
    `MODEL res | _ ->
  match dest_decouple_ext expr with Some res ->
    `DECOUPLE res
  | _ ->
    `NOEXT

let rewriter _ _ =
  { default_mapper with
    expr = fun mapper ({ pexp_loc = loc } as expr) ->
      match dest_ext expr with
      | `PC (None, None) ->
        prov_const expr.pexp_loc
      | `PC (Some [(None, (init, loc))], None) ->
        prov_const ~init loc
      | `PC (Some bindings, cont) ->
        let bindings = 
          List.map 
            (fun (vb, (init, loc)) -> 
              let vb = Option.get_exn vb in
              { vb with pvb_expr = prov_const ~init loc }) 
            (bindings) in
        let cont = mapper.expr mapper (Option.get_exn cont) in
        Exp.let_ ~loc Nonrecursive bindings cont
      | `LIFT ([(None, (Some e, loc))], None) ->
        lift e loc
      | `LIFT (bindings, cont) ->
        let bindings = 
          List.map 
            (fun (vb, (init, loc)) -> 
              let { pvb_expr } as vb = Option.get_exn vb in
              let pvb_expr = lift pvb_expr loc in
              { vb with pvb_expr }) 
            (bindings) in
        let cont = mapper.expr mapper (Option.get_exn cont) in
        Exp.let_ ~loc Nonrecursive bindings cont
      | `DECOUPLE (bindings, cont) ->
        let bindings =
          List.map
            (fun ({ pvb_expr } as pvb) ->
              let model = mapper.expr mapper pvb_expr in
              decouple pvb model)
            (bindings) in
        let cont = mapper.expr mapper cont in
        Exp.let_ ~loc Nonrecursive bindings cont
      | _ -> 
        default_mapper.expr mapper expr ; }

let () =
  Driver.register ~name:"decml" ~reset_args ~args
    Versions.ocaml_404 rewriter