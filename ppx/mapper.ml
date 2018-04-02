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
          { pexp_desc = Pexp_let (Recursive, 
              [{ pvb_pat = { ppat_desc = Ppat_var { txt } } } as pvb], cont) },
        _)}] ->
      Some (`LETREC (txt, pvb, cont))
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Recursive, 
              [{ pvb_pat = { ppat_desc = _ }; pvb_loc }], _) },
        _)}] ->
        Err.pattern pvb_loc
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Recursive, _, _); pexp_loc },
        _)}] ->
      Err.rec_model pexp_loc
    | PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Nonrecursive, bindings, cont) }, 
        _)}] ->
      let () =
        List.iter
          (function
            | { pvb_pat = { ppat_desc = Ppat_var _ } } -> ()
            | { pvb_loc }                              -> Err.pattern pvb_loc)
          (bindings) in
      Some (`LET (bindings, cont))
    | PStr [{ pstr_desc = Pstr_eval (e, _) } ] ->
      Some (`IMMEDIATE e)
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
    `UNKNOWNEXT

let rec rewriter config cookies scope =

  let ident { txt } ({ pexp_loc } as e) =
    let idx = Option.map fst (List.find_idx (Longident.equal txt) scope) in
    match idx with
    | None ->
      (* If it is not in the scope, it is a free variable and we assume it is
          a model. *)
      let e =
        Exp.open_ Override (mknoloc Overlay.Pervasives._module_name) e in
      let pexp_loc = { pexp_loc with loc_ghost = true } in
      weaken (List.length scope) e pexp_loc
    | Some idx ->
      (* The identifier is a variable bound in the model. *)
      var idx (List.length scope) pexp_loc

  and apply self e args pexp_loc =
    let e = self.expr self e in
    let args = 
      List.map (fun (lbl, e) -> lbl, self.expr self e) (args) in
    let pexp_loc = { pexp_loc with loc_ghost = true } in
    let app = List.fold_left (fun e (lbl, e') -> apply ~lbl e e') e args in
    { app with pexp_loc }

  and abs (lbl, default, pat, body) pexp_loc =
    match lbl, default, pat with
    | Nolabel, None, { ppat_desc = Ppat_var { txt }} ->
      let self = rewriter config cookies ((Longident.Lident txt) :: scope) in
      let body = self.expr self body in
      abs body pexp_loc
    | Nolabel, None, _ ->
      Location.raise_errorf ~loc:pexp_loc
        "Expecting a singleton variable pattern!"
    | _ ->
      Location.raise_errorf ~loc:pexp_loc
        "Labelled arguments not supported in models!"

  and let_bind self pvbs cont pexp_loc =
    let scope' =
      List.fold_left
        (fun scope pvb ->
          match pvb with
          | { pvb_pat = { ppat_desc = Ppat_var { txt } } } ->
            (Longident.Lident txt) :: scope
          | { pvb_loc } ->
            Err.pattern pvb_loc)
        (scope)
        (pvbs) in
    let es = 
      List.mapi
        (fun idx { pvb_expr = ({ pexp_loc } as e) } -> 
          let e = self.expr self e in
          weaken idx e pexp_loc)
        (pvbs) in
    let self = rewriter config cookies scope' in
    let cont = self.expr self cont in
    let_bind es cont pexp_loc

  and let_rec txt cont loc =
    let scope = (Longident.Lident txt) :: scope in
    let self = rewriter config cookies scope in
    let cont = self.expr self cont in
    let loc = { loc with loc_ghost = true } in
    Exp.apply ~loc (mk_ident Model.abs_rec) [ Nolabel, cont]

  and pair self e es pexp_loc =
    let es = List.map (self.expr self) es in
    pair e es pexp_loc

  in
  { Err.mapper with 
      expr = fun self ->
        function
        | { pexp_desc = Pexp_ident id } as e ->
          ident id e
        | { pexp_desc = Pexp_constant _; pexp_loc } as e ->
          lift e (List.length scope) pexp_loc
        | { pexp_desc = Pexp_let (Nonrecursive, pvbs, cont); pexp_loc } ->
          let_bind self pvbs cont pexp_loc
        | { pexp_desc = 
              Pexp_let ( Recursive, 
                [{ pvb_pat = { ppat_desc = Ppat_var { txt } } }],  
                cont) ;
              pexp_loc } ->
          let_rec txt cont pexp_loc
        | { pexp_desc = 
              Pexp_let (Recursive, [{ pvb_pat = { ppat_desc = _ }; 
                                      pvb_loc }], _) } ->
          Err.pattern pvb_loc
        | { pexp_desc = Pexp_let (Recursive, _, _); pexp_loc } ->
          Err.rec_model pexp_loc
        | { pexp_desc = Pexp_fun (lbl, default, pat, body); pexp_loc } ->
          abs (lbl, default, pat, body) pexp_loc
        | { pexp_desc = Pexp_apply (e, args); pexp_loc } ->
          apply self e args pexp_loc
        | { pexp_desc = Pexp_tuple es; pexp_loc } as e ->
          begin match es with
          | [ _; _ ] ->
            pair self e es pexp_loc
          | _ ->
            (* Compiler enforces invariant that [es] >= 2 *)
            Location.raise_errorf ~loc:pexp_loc
              "Only 2-tuples supported!"
          end
        | { pexp_desc = Pexp_construct _ } ->
          failwith "Not implemented!"
        | { pexp_desc = Pexp_ifthenelse _ } ->
          failwith "Not implemented!"
        | { pexp_desc = Pexp_extension _ } ->
          failwith "Not implemented!"
        | { pexp_loc } ->
          Err.unsupported_model pexp_loc
        ;
    }

let model_rewriter config cookies =
 rewriter config cookies []

let rec_model_rewriter config cookies id =
  rewriter config cookies [ Longident.Lident id ]

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
        lift e 0 loc

      | `LIFT (bindings, cont) ->
        let bindings = 
          List.map 
            (fun (vb, (_, loc)) -> 
              let { pvb_expr } as vb = Option.get_exn vb in
              let pvb_expr = lift pvb_expr 0 loc in
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

      | `MODEL `IMMEDIATE e ->
        model_mapper.expr model_mapper e

      | `MODEL `LET (bindings, cont) ->
        let bindings =
          List.map
            (fun ({ pvb_expr } as vb) ->
              let pvb_expr = model_mapper.expr model_mapper pvb_expr in
              { vb with pvb_expr }) 
          (bindings) in
        let cont = self.expr self cont in
        Exp.let_ ~loc Nonrecursive bindings cont
      
      | `MODEL `LETREC (f, ({ pvb_expr } as pvb), cont) ->
        let mapper = rec_model_rewriter config cookies f in
        let pvb_expr = mapper.expr mapper pvb_expr in
        let cont = self.expr self cont in
        Exp.let_ ~loc Nonrecursive [ { pvb with pvb_expr } ] cont

      | `UNKNOWNEXT -> 
        default_mapper.expr self expr ; }

let () =
  Driver.register ~name:"decml" ~reset_args ~args
    Versions.ocaml_404 rewriter