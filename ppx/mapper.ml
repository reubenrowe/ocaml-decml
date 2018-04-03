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

let dest_pc_ext req_cont ext loc =
  match ext with
  | { txt }, payload when is_pc_ext txt ->
    begin match req_cont, payload with
    | _, PStr [] ->
      Some (None, None)
    | _, PStr [{ pstr_desc = Pstr_eval ( 
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
    | _, PStr [{ pstr_desc = Pstr_eval (({ pexp_loc } as e), _) } ] ->
      Some (Some [(None, (dest_exp_const e, pexp_loc))], None)
    | false, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      let bindings =
        List.map
          (function
            | { pvb_pat = { ppat_desc = Ppat_var _ }; pvb_expr } as pvb ->
              Some pvb, (dest_exp_const pvb_expr, pvb_expr.pexp_loc)
            | { pvb_loc } ->
              Err.pattern pvb_loc)
          (bindings) in
      Some (Some bindings, None)
    | true, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      Err.continuation loc
    | _ ->
      Err.payload loc
    end
  | _ ->
    None

let dest_lift_ext req_cont ext loc =
  match ext with
  | { txt }, payload when is_lift_ext txt ->
    begin match req_cont, payload with
    | _, PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (_, bindings, cont) }, 
        _)}] ->
      let bindings =
        List.map
          (function
            | { pvb_pat = { ppat_desc = Ppat_var _ }; 
                pvb_expr; pvb_loc } as pvb ->
              Some pvb, (Some pvb_expr, pvb_loc)
            | { pvb_loc } ->
              Err.pattern pvb_loc)
          (bindings) in
      Some (bindings, Some cont)
    | _, PStr [{ pstr_desc = Pstr_eval (({ pexp_loc } as e), _) } ] ->
      Some ([(None, (Some e, pexp_loc))], None)
    | false, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      let bindings =
        List.map
          (function
          | { pvb_pat = { ppat_desc = Ppat_var _ }; pvb_loc } as pvb ->
            Some pvb, (None, pvb_loc)
          | { pvb_loc } ->
            Err.pattern pvb_loc)
        (bindings) in
      Some (bindings, None)
    | true, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      Err.continuation loc
    | _ ->
      Err.payload loc
    end
  | _ ->
    None

let dest_model_ext req_cont ext loc =
  let check_bindings bindings =
    List.iter
      (function
        | { pvb_pat = { ppat_desc = Ppat_var _ } } -> ()
        | { pvb_loc }                              -> Err.pattern pvb_loc)
      (bindings) in
  match ext with
  | { txt }, payload when is_model_ext txt ->
    begin match req_cont, payload with
    | _, PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Recursive, 
              [{ pvb_pat = { ppat_desc = Ppat_var { txt } } } as pvb], cont) },
        _)}] ->
      Some (`LETREC (txt, pvb, Some cont))
    | false, 
      PStr [{ pstr_desc = Pstr_value (Recursive, 
                [{ pvb_pat = { ppat_desc = Ppat_var { txt } } } as pvb]
              )}] ->
      Some (`LETREC (txt, pvb, None))
    | _, PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Recursive, 
              [{ pvb_pat = { ppat_desc = _ }; pvb_loc }], _) },
        _)}]
    | false, 
      PStr [{ pstr_desc = Pstr_value (Recursive, 
                [{ pvb_pat = { ppat_desc = _ }; pvb_loc }]
              )}] ->
      Err.pattern pvb_loc
    | _, PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Recursive, _, _); pexp_loc = loc },
        _)}]
    | false, PStr [{ pstr_desc = Pstr_value (Recursive, _ ); pstr_loc = loc}] ->
      Err.rec_model loc
    | _, PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (Nonrecursive, bindings, cont) }, 
        _)}] ->
      let () = check_bindings bindings in
      Some (`LET (bindings, Some cont))
    | false, PStr [{ pstr_desc = Pstr_value (Nonrecursive, bindings)}] ->
      let () = check_bindings bindings in
      Some (`LET (bindings, None))
    | _, PStr [{ pstr_desc = Pstr_eval (e, _) } ] ->
      Some (`IMMEDIATE e)
    | true, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      Err.continuation loc
    | _ ->
      Err.payload loc
    end
  | _ ->
    None

let dest_decouple_ext req_cont ext loc =
  let check_bindings bindings = 
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
  match ext with
  | { txt }, payload when is_decouple_ext txt ->
    begin match req_cont, payload with
    | true, PStr [{ pstr_desc = Pstr_eval ( 
          { pexp_desc = Pexp_let (_, bindings, cont) }, 
        _)}] ->
      let () = check_bindings bindings in
      Some (bindings, Some cont)
    | false, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      let () = check_bindings bindings in
      Some (bindings, None)
    | true, PStr [{ pstr_desc = Pstr_value (_, bindings)}] ->
      Err.continuation loc
    | _ ->
      Err.payload loc
    end
  | _ ->
    None
        
let dest_ext ?(req_cont=true) ext loc =
  match dest_pc_ext req_cont ext loc       with Some res ->
    `PC res | _ ->
  match dest_lift_ext req_cont ext loc     with Some res ->
    `LIFT res | _ ->
  match dest_model_ext req_cont ext loc    with Some res ->
    `MODEL res | _ ->
  match dest_decouple_ext req_cont ext loc with Some res ->
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

  and let_bind map bindings cont pexp_loc =
    let scope' =
      List.fold_left
        (fun scope (_, txt) -> (Longident.Lident txt) :: scope)
        (scope)
        (bindings) in
    let es = 
      List.mapi
        (fun idx ((v, loc), _) -> weaken idx (map v) loc)
        (bindings) in
    let self = rewriter config cookies scope' in
    let cont = self.expr self cont in
    let_bind es cont pexp_loc

  and let_rec f body cont loc =
    let self = rewriter config cookies ((Longident.Lident f) :: scope) in
    let body = self.expr self body in
    let cont = self.expr self cont in
    letrec_bind (List.length scope) body cont loc

  and pair self e es pexp_loc =
    let es = List.map (self.expr self) es in
    pair e es pexp_loc

  and ifthenelse self (_test, _then, _else) pexp_loc =
    let _test = self.expr self _test in
    let _then = self.expr self _then in
    let _else = self.expr self _else in
    ifthenelse (_test, _then, _else) pexp_loc
    
  in
  { Err.mapper with 
      expr = fun self ->
        function
        | { pexp_desc = Pexp_ident id } as e ->
          ident id e
        | { pexp_desc = Pexp_constant _; pexp_loc } as e ->
          lift e (List.length scope) pexp_loc
        | { pexp_desc = Pexp_let (Nonrecursive, pvbs, cont); pexp_loc } ->
          let bindings =
            List.map
              (function
                | { pvb_pat = { ppat_desc = Ppat_var { txt } }; 
                    pvb_expr = { pexp_loc } as e } ->
                  (e, pexp_loc), txt
                | { pvb_loc } ->
                  Err.pattern pvb_loc)
              (pvbs) in
          let_bind (self.expr self) bindings cont pexp_loc
        | { pexp_desc = 
              Pexp_let (Recursive, 
                [{ pvb_pat = { ppat_desc = Ppat_var { txt } }; pvb_expr }],  
                cont) ;
              pexp_loc } ->
          let_rec txt pvb_expr cont pexp_loc
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
        | { pexp_desc = Pexp_construct _; pexp_loc } ->
          Location.raise_errorf ~loc:pexp_loc "Not implemented!"
        | { pexp_desc = Pexp_ifthenelse (_test, _then, Some _else); 
            pexp_loc } ->
          ifthenelse self (_test, _then, _else) pexp_loc
        | { pexp_desc = Pexp_ifthenelse (_, _, None); pexp_loc } ->
          Err.ifthenelse pexp_loc
        | { pexp_desc = Pexp_extension ext ; pexp_loc } as expr ->
          begin match dest_ext ext pexp_loc with
          | `PC (None, Some _) ->
            assert false
          | `PC (None, None) ->
            weaken (List.length scope) (prov_const Location.none) pexp_loc
          | `PC (Some [(None, (init, loc))], None) ->
            weaken (List.length scope) (prov_const ~init Location.none) loc
          | `PC (Some bindings, cont) ->
            let bindings = 
              List.map 
                (fun (vb, payload) -> 
                  match Option.get_exn vb with
                  | { pvb_pat = { ppat_desc = Ppat_var { txt } } } ->
                    payload, txt
                  | _ -> assert false) 
                (bindings) in
            let_bind 
              (fun init -> 
                let noloc = Location.none in
                weaken (List.length scope) (prov_const ~init noloc) noloc)
              (bindings)
              (Option.get_exn cont) 
              (pexp_loc)
    
          | `LIFT ([(None, (Some e, loc))], None) ->
            lift e (List.length scope) loc
          | `LIFT (bindings, cont) ->
            let bindings = 
              List.map 
                (fun (vb, payload) -> 
                  match Option.get_exn vb with
                  | { pvb_pat = { ppat_desc = Ppat_var { txt } } } ->
                    (Pair.map1 Option.get_exn payload), txt
                  | _ -> assert false) 
                (bindings) in
            let_bind
              (fun e -> lift e (List.length scope) Location.none)
              (bindings)
              (Option.get_exn cont) 
              (pexp_loc)

          | `MODEL `IMMEDIATE e ->
            self.expr self e
          | `MODEL `LET (bindings, cont) ->
            let bindings =
              List.map
                (function
                  | { pvb_pat = { ppat_desc = Ppat_var { txt } }; 
                      pvb_expr = { pexp_loc } as e } ->
                    (e, pexp_loc), txt
                  | _ -> assert false) 
              (bindings) in
            let_bind (self.expr self) bindings (Option.get_exn cont) pexp_loc
          | `MODEL `LETREC (f, { pvb_expr }, cont) ->
            let_rec f pvb_expr (Option.get_exn cont) pexp_loc
              
          | `DECOUPLE _ ->
            Err.nested_decouple pexp_loc
            
          | `UNKNOWNEXT ->
            expr
          end
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
      expr = 
        begin fun self ->
          function
          | { pexp_desc = Pexp_extension ext; pexp_loc = loc } as expr ->
            begin match dest_ext ext loc with

            | `PC (None, Some _) ->
              assert false

            | `PC (None, None) ->
              prov_const loc

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
              let cont = self.expr self (Option.get_exn cont) in
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
              let cont = self.expr self (Option.get_exn cont) in
              Exp.let_ ~loc Nonrecursive bindings cont
            
            | `MODEL `LETREC (f, ({ pvb_expr } as pvb), cont) ->
              let mapper = rec_model_rewriter config cookies f in
              let { pexp_loc } as pvb_expr = mapper.expr mapper pvb_expr in
              let pvb_expr = { pvb_expr with pexp_loc = Location.none } in
              let pvb_expr = abs_rec 0 pvb_expr pexp_loc in
              let cont = self.expr self (Option.get_exn cont) in
              Exp.let_ ~loc Nonrecursive [ { pvb with pvb_expr } ] cont

            | `UNKNOWNEXT -> 
              expr
            end
          | expr ->
            default_mapper.expr self expr 
        end ;
      structure_item =
        begin fun self ->
          function
          | { pstr_desc = Pstr_extension (ext, _); pstr_loc = loc } ->
            begin match dest_ext ~req_cont:false ext loc with
            | `PC (Some bindings, None) ->
              let bindings = 
                List.map 
                  (fun (vb, (init, loc)) -> 
                    let vb = Option.get_exn vb in
                    { vb with pvb_expr = prov_const ~init loc }) 
                  (bindings) in
              Str.value ~loc Nonrecursive bindings
            | `LIFT (bindings, None) ->
              let bindings = 
                List.map 
                  (fun (vb, (_, loc)) -> 
                    let { pvb_expr } as vb = Option.get_exn vb in
                    let pvb_expr = lift pvb_expr 0 loc in
                    { vb with pvb_expr }) 
                  (bindings) in
              Str.value ~loc Nonrecursive bindings
            | `DECOUPLE (bindings, None) ->
              let bindings =
                List.map
                  (fun ({ pvb_expr } as pvb) ->
                    let model = self.expr self pvb_expr in
                    decouple pvb model)
                  (bindings) in
              Str.value ~loc Nonrecursive bindings
            | `MODEL `LET (bindings, None) ->
              let bindings =
                List.map
                  (fun ({ pvb_expr } as vb) ->
                    let pvb_expr = model_mapper.expr model_mapper pvb_expr in
                    { vb with pvb_expr }) 
                (bindings) in
              Str.value ~loc Nonrecursive bindings
            
            | `MODEL `LETREC (f, ({ pvb_expr } as pvb), cont) ->
              let mapper = rec_model_rewriter config cookies f in
              let { pexp_loc } as pvb_expr = mapper.expr mapper pvb_expr in
              let pvb_expr = { pvb_expr with pexp_loc = Location.none } in
              let pvb_expr = abs_rec 0 pvb_expr pexp_loc in
              Str.value ~loc Nonrecursive [ { pvb with pvb_expr } ]

            | _ ->
              assert false
            end
          | item ->
            default_mapper.structure_item self item
        end ; 
  }

let () =
  Driver.register ~name:"decml" ~reset_args ~args
    Versions.ocaml_404 rewriter