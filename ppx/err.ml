open Migrate_parsetree
open   Ast_404
open     Ast_helper
open     Ast_mapper
open     Asttypes
open     Location
open     Longident
open     Parsetree

let pattern loc =
  Location.raise_errorf ~loc "Expecting a singleton variable pattern!"
let payload loc =
  Location.raise_errorf ~loc "Expecting a single value or binding!"
let const_exp loc = 
  Location.raise_errorf ~loc "Expecting a single constant expression!"
let decouple loc =
  Location.raise_errorf ~loc "Expecting a variable pair pattern!"
let rec_model loc =
  Location.raise_errorf ~loc "Mutual recursion not supported for models!"

let unsupported_model loc = 
  Location.raise_errorf ~loc "Not supported in models!"

let mapper : mapper = {
    attribute               = (fun _ ({ loc }, _) -> unsupported_model loc) ;
    attributes              = (fun _ -> function | [] -> [] | ({ loc }, _) :: _ -> unsupported_model loc) ;
    case                    = (fun _ { pc_lhs = { ppat_loc } } -> unsupported_model ppat_loc) ;
    cases                   = (fun _ -> function | [] -> [] | { pc_lhs = { ppat_loc } } :: _ -> unsupported_model ppat_loc) ;
    class_declaration       = (fun _ { pci_loc } -> unsupported_model pci_loc) ;
    class_description       = (fun _ { pci_loc } -> unsupported_model pci_loc) ;
    class_expr              = (fun _ { pcl_loc } -> unsupported_model pcl_loc) ;
    class_field             = (fun _ { pcf_loc } -> unsupported_model pcf_loc) ;
    class_signature         = (fun _ { pcsig_self = { ptyp_loc } } -> unsupported_model ptyp_loc) ;
    class_structure         = (fun _ { pcstr_self = { ppat_loc } } -> unsupported_model ppat_loc) ;
    class_type              = (fun _ { pcty_loc } -> unsupported_model pcty_loc) ;
    class_type_declaration  = (fun _ { pci_loc } -> unsupported_model pci_loc) ;
    class_type_field        = (fun _ { pctf_loc } -> unsupported_model pctf_loc) ;
    constructor_declaration = (fun _ { pcd_loc } -> unsupported_model pcd_loc) ;
    expr                    = (fun _ { pexp_loc } -> unsupported_model pexp_loc) ;
    extension               = (fun _ ({ loc }, _) -> unsupported_model loc) ;
    extension_constructor   = (fun _ { pext_loc } -> unsupported_model pext_loc) ;
    include_declaration     = (fun _ { pincl_loc } -> unsupported_model pincl_loc) ;
    include_description     = (fun _ { pincl_loc } -> unsupported_model pincl_loc) ;
    label_declaration       = (fun _ { pld_loc } -> unsupported_model pld_loc) ;
    location                = (fun _ -> unsupported_model) ;
    module_binding          = (fun _ { pmb_loc } -> unsupported_model pmb_loc) ;
    module_declaration      = (fun _ {pmd_loc } -> unsupported_model pmd_loc) ;
    module_expr             = (fun _ { pmod_loc } -> unsupported_model pmod_loc) ;
    module_type             = (fun _ { pmty_loc } -> unsupported_model pmty_loc) ;
    module_type_declaration = (fun _ { pmtd_loc } -> unsupported_model pmtd_loc) ;
    open_description        = (fun _ { popen_loc } -> unsupported_model popen_loc) ;
    pat                     = (fun _ { ppat_loc } -> unsupported_model ppat_loc) ;
    signature               = (fun _ -> function | [] -> [] | { psig_loc } :: _ -> unsupported_model psig_loc) ;
    signature_item          = (fun _ { psig_loc } -> unsupported_model psig_loc) ;
    structure               = (fun _ -> function | [] -> [] | { pstr_loc } :: _ -> unsupported_model pstr_loc) ;
    structure_item          = (fun _ { pstr_loc } -> unsupported_model pstr_loc) ;
    typ                     = (fun _ { ptyp_loc } -> unsupported_model ptyp_loc) ;
    type_declaration        = (fun _ { ptype_loc } -> unsupported_model ptype_loc) ;
    type_extension          = (fun _ { ptyext_path = { loc } } -> unsupported_model loc) ;
    value_binding           = (fun _ { pvb_loc } -> unsupported_model pvb_loc) ;
    value_description       = (fun _ { pval_loc } -> unsupported_model pval_loc) ;
    type_kind               = 
      (fun _ -> 
        function
        | Ptype_abstract                   -> Ptype_abstract
        | Ptype_open                       -> Ptype_open
        | Ptype_variant []                 -> Ptype_variant []
        | Ptype_variant ({ pcd_loc } :: _) -> unsupported_model pcd_loc
        | Ptype_record []                  -> Ptype_record []
        | Ptype_record ({ pld_loc  }:: _)  -> unsupported_model pld_loc) ;
    with_constraint         = 
      (fun _ ->
        function
        | Pwith_type ({ loc }, _) -> unsupported_model loc
        | Pwith_module ({ loc }, _) -> unsupported_model loc
        | Pwith_typesubst { ptype_loc } -> unsupported_model ptype_loc
        | Pwith_modsubst ({ loc }, _) -> unsupported_model loc) ;
    payload                 = 
      (fun _ -> 
        function 
        | PStr [] -> PStr [] 
        | PSig [] -> PSig []
        | PStr ({ pstr_loc } :: _) -> unsupported_model pstr_loc
        | PSig ({ psig_loc } :: _) -> unsupported_model psig_loc
        | PTyp { ptyp_loc }        -> unsupported_model ptyp_loc
        | PPat ( { ppat_loc }, _ ) -> unsupported_model ppat_loc);
  } 