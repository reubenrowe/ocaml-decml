open Containers

open Migrate_parsetree
open   Ast_404
open     Ast_helper
open     Ast_mapper
open     Asttypes
open     Location
open     Parsetree

module Longident = struct
  include Longident
  let equal lid lid' = 
    List.equal String.equal (flatten lid) (flatten lid')
end

open Build
open Identifiers

[@@@warning "-23"]
(* Disable warning: all the fields are explicitly used in this record. *)

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
    begin match payload with
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Recursive, _, _) },
        _)}] ->
      Err.rec_model pexp_loc
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (_, bindings, cont) }, 
        _)}] ->
      let bindings =
        List.map
          (function
            | { pvb_pat = { ppat_desc = Ppat_var _ }; pvb_loc } as pvb ->
              Some pvb, None
            | { pvb_loc } ->
              Err.pattern pvb_loc)
          (bindings) in
      Some (bindings, Some cont)
    | PStr [{ pstr_desc = Pstr_eval (e, _) } ] ->
      Some ([(None, Some e)], None)
    | _ ->
      Err.payload pexp_loc
    end
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

let model_rewriter _ _ =
  let rec rewriter scope =
    let ident { txt } ({ pexp_loc } as e) =
      let idx = Option.map fst (List.find_idx (Longident.equal txt) scope) in
      match idx with
      | None ->
        (* If it is not in the scope, it is a free variable and we assume it is
           a model. *)
        Attribute.(add { default with depth = 0 } e)
      | Some idx ->
        (* The identifier is a variable bound in the model. *)
        let e = var idx pexp_loc in
        Attribute.(add { default with depth = idx + 1} e)
    and apply e args max_ctxt pexp_loc =
      let Attribute.{ depth } = Attribute.get e in
      let e = weaken (max_ctxt - depth) (Attribute.remove e) in
      let args =
        List.map
          (fun (lbl, e) ->
            let Attribute.{ depth } = Attribute.get e in
            lbl, weaken (max_ctxt - depth) (Attribute.remove e))
          (args) in
      let app = List.fold_left (fun e (lbl, e') -> apply ~lbl e e') e args in
      let app = { app with pexp_loc } in
      Attribute.(add { default with depth = max_ctxt } app)
    in
    { Err.mapper with 
        expr = fun self ->
          function
          | { pexp_desc = Pexp_ident id } as e ->
            ident id e
          | { pexp_desc = Pexp_constant _; pexp_loc } as e ->
            lift e pexp_loc
          | { pexp_desc = Pexp_let _ } ->
            failwith "Not implemented!"
          | { pexp_desc = Pexp_fun (lbl, default, pat, body); pexp_loc } ->
            begin match lbl, default, pat with
            | Nolabel, None, { ppat_desc = Ppat_var { txt }} ->
              let self = rewriter ((Longident.Lident txt) :: scope) in
              let body = self.expr self body in
              let Attribute.{ depth } = Attribute.get body in
              let body = Attribute.remove body in
              let e = abs body pexp_loc in
              let depth = depth - 1 in
              Attribute.(add { default with depth } e) 
            | Nolabel, None, _ ->
              Location.raise_errorf ~loc:pexp_loc
                "Expecting a singleton variable pattern!"
            | _ ->
              Location.raise_errorf ~loc:pexp_loc
                "Labelled arguments not supported in models!"
            end
          | { pexp_desc = Pexp_apply (e, args); pexp_loc } ->
            let e = self.expr self e in
            let Attribute.{ depth } = Attribute.get e in
            let max_ctxt, args = 
              List.fold_map 
                (fun max_ctxt (lbl, e) -> 
                  let e = self.expr self e in
                  let Attribute.{ depth } = Attribute.get e in
                  (max max_ctxt depth), (lbl, e)) 
                depth (args) in
            apply e args max_ctxt pexp_loc
          | { pexp_desc = Pexp_tuple _ } ->
            failwith "Not implemented!"
          | { pexp_desc = Pexp_construct _ } ->
            failwith "Not implemented!"
          | { pexp_desc = Pexp_ifthenelse _ } ->
            failwith "Not implemented!"
          | { pexp_desc = Pexp_extension _ } ->
            failwith "Not implemented!"
          | { pexp_loc } ->
            Err.unsupported_model pexp_loc
          ;
     } in
  rewriter []

let rewriter config cookies =
  let model_mapper = model_rewriter config cookies in
  { default_mapper with
    expr = fun self ({ pexp_loc = loc } as expr) ->
      match dest_ext expr with

      | `PC (None, Some _) ->
        assert false

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
        let cont = self.expr self (Option.get_exn cont) in
        Exp.let_ ~loc Nonrecursive bindings cont

      | `LIFT ([(None, (Some e, loc))], None) ->
        lift e loc

      | `LIFT (bindings, cont) ->
        let bindings = 
          List.map 
            (fun (vb, (_, loc)) -> 
              let { pvb_expr } as vb = Option.get_exn vb in
              let pvb_expr = lift pvb_expr loc in
              { vb with pvb_expr }) 
            (bindings) in
        let cont = self.expr self (Option.get_exn cont) in
        Exp.let_ ~loc Nonrecursive bindings cont

      | `DECOUPLE (bindings, cont) ->
        let bindings =
          List.map
            (fun ({ pvb_expr } as pvb) ->
              let model = self.expr self pvb_expr in
              decouple pvb model)
            (bindings) in
        let cont = self.expr self cont in
        Exp.let_ ~loc Nonrecursive bindings cont

      | `MODEL ([None, Some e], None) ->
        let model = 
          Attribute.remove (model_mapper.expr model_mapper e) in
        Exp.open_ Override (mknoloc Overlay.Pervasives._module_name) model

      | `MODEL (bindings, cont) ->
        let bindings =
          List.map
            (function
              | (Some ({ pvb_expr } as vb), None) ->
                let pvb_expr = 
                  Attribute.remove (model_mapper.expr model_mapper pvb_expr) in
                let pvb_expr = 
                  Exp.open_ Override (mknoloc Overlay.Pervasives._module_name) 
                    pvb_expr in
                { vb with pvb_expr }
              | _ -> assert false) 
          (bindings) in
        let cont = self.expr self (Option.get_exn cont) in
        Exp.let_ ~loc Nonrecursive bindings cont
        
      | `NOEXT -> 
        default_mapper.expr self expr ; }

let () =
  Driver.register ~name:"decml" ~reset_args ~args
    Versions.ocaml_404 rewriter