(* Magic values for the DecML PPX *)

open Containers

open Migrate_parsetree
open   Ast_404
open     Ast_helper
open     Location
open     Longident

(* Extension node names *)

let ext_model = "model"
let ext_pc    = "pc"
let ext_lift  = "lift"

let is_model_ext =
  String.equal ext_model
let is_pc_ext =
  String.equal ext_pc
let is_lift_ext =
  String.equal ext_lift

let mk_ident lid = Exp.ident (Location.mknoloc lid)

(* OCaml Native Identifiers *)
let unit = Lident "()"

(* Library Modules and Values *)

let lib_name = Lident "Decml"

module Model = struct
  let _module_name = Ldot (lib_name, "Model")
  let _Lifted = Ldot (_module_name, "Lifted")
  let _Ex = Ldot (_module_name, "Ex")
  module Parameters = struct
    let _module_name = Ldot (_module_name, "Parameters")
    let null = Ldot (_module_name, "null")
  end
  module Overlay = struct
    let _module_name = Ldot (_module_name, "Overlay")
    module Pervasives = struct
      let _module_name = Ldot (_module_name, "Pervasives")
    end
    module List = struct
      let _module_name = Ldot (_module_name, "List")
    end
  end
  let pc       = Ldot (_module_name, "pc")
  let var      = Ldot (_module_name, "var")
  let app      = Ldot (_module_name, "app")
  let abs      = Ldot (_module_name, "abs")
  let let_bind = Ldot (_module_name, "let_bind")
  let tx       = Ldot (_module_name, "tx")
  let pair     = Ldot (_module_name, "pair")
  let unit     = Ldot (_module_name, "unit")
  let _true    = Ldot (_module_name, "_true")
  let _false   = Ldot (_module_name, "_false")
  let ifelse   = Ldot (_module_name, "ifelse")
  let abs_rec  = Ldot (_module_name, "abs_rec")
end

