(* Magic values for the DecML PPX *)

open Containers

open Migrate_parsetree
open   Ast_404
open     Ast_helper
open     Longident

(* Extension node names *)

let ext_model    = "model"
let ext_pc       = "pc"
let ext_lift     = "lift"
let ext_decouple = "decouple"

let is_model_ext s =
  String.equal ext_model s
let is_pc_ext s =
  String.equal ext_pc s
let is_lift_ext s =
  String.equal ext_lift s
let is_decouple_ext s =
  String.equal ext_decouple s

let mk_ident ?loc lid = Exp.ident ?loc (Location.mknoloc lid)

(* OCaml Native Identifiers *)
let unit = Lident "()"

(* Library Modules and Values *)

let lib_name = Lident "Decml"

module Model = struct
  let _module_name = Ldot (lib_name, "Model")
  let _Weak = Ldot (_module_name, "Weak")
  let _Exch = Ldot (_module_name, "Exch")
  let _Cong = Ldot (_module_name, "Cong")
  let _Lifted = Ldot (_module_name, "Lifted")
  let _Ex = Ldot (_module_name, "Ex")
  module Parameters = struct
    let _module_name = Ldot (_module_name, "Parameters")
    let null = Ldot (_module_name, "null")
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
  let fmap     = Ldot (_module_name, "fmap")
end

module Overlay = struct
  let _module_name = Ldot (lib_name, "Overlay")
  module Pervasives = struct
    let _module_name = Ldot (_module_name, "Pervasives")
  end
  module List = struct
    let _module_name = Ldot (_module_name, "List")
  end
end
