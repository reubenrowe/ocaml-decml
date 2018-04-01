open Containers

open Migrate_parsetree
open   Ast_404
open     Ast_helper
open     Asttypes
open     Location
open     Longident
open     Parsetree


let attr_id = "decml"

type payload = {
    depth : int ;
  }

let default = {
    depth = 0 ;
  }

let field_err f =
  failwith 
    (Format.sprintf "Field %s not found or contains unexpected payload!" f)

let get { pexp_attributes } = 
  let attr =
    List.find_map
      (fun ({ txt }, payload) -> 
        if String.equal attr_id txt then Some payload else None)
      (pexp_attributes) in
  match attr with
  | Some PStr [ 
        { pstr_desc 
            = Pstr_eval ({ pexp_desc = Pexp_record (fields, None) }, _) } 
      ] ->
    let depth =
      let depth = 
        List.find_map 
          (fun ({ txt }, e) -> 
            match txt with
            | Lident f when String.equal f "depth" -> Some e
            | _ -> None) 
          (fields) in
      match depth with
      | Some { pexp_desc = Pexp_constant (Pconst_integer (d, _)) } ->
        int_of_string d
      | _ ->
        field_err "depth" in
    { depth; } 
  | _ ->
    raise Not_found

let remove ({ pexp_attributes } as e) =
  let pexp_attributes =
    List.filter 
      (fun ({ txt }, _) -> not (String.equal attr_id txt))
      (pexp_attributes) in
  { e with pexp_attributes }

let mk_fieldname f =
  Location.mknoloc (Lident f)

let add attr ({ pexp_attributes } as e) =
  let { depth } = attr in
  let fields =
    let depth = (mk_fieldname "depth"), Exp.constant (Const.int depth) in
    [ depth; ] in
  let attr = 
    (mknoloc attr_id), 
    (PStr [ Str.mk (Pstr_eval (Exp.record fields None, [])) ]) in
  let pexp_attributes = attr :: pexp_attributes in
  { e with pexp_attributes }
