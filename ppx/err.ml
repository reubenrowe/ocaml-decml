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

let mapper loc = 
  Location.raise_errorf ~loc "Not supported in models!"

let mapper : mapper = {
    attribute               = (fun _ ({ loc }, _) -> mapper loc) ;
    attributes              = (fun _ -> function | [] -> [] | ({ loc }, _) :: _ -> mapper loc) ;
    case                    = (fun _ { pc_lhs = { ppat_loc } } -> mapper ppat_loc) ;
    cases                   = (fun _ -> function | [] -> [] | { pc_lhs = { ppat_loc } } :: _ -> mapper ppat_loc) ;
    class_declaration       = (fun _ { pci_loc } -> mapper pci_loc) ;
    class_description       = (fun _ { pci_loc } -> mapper pci_loc) ;
    class_expr              = (fun _ { pcl_loc } -> mapper pcl_loc) ;
    class_field             = (fun _ { pcf_loc } -> mapper pcf_loc) ;
    class_signature         = (fun _ { pcsig_self = { ptyp_loc } } -> mapper ptyp_loc) ;
    class_structure         = (fun _ { pcstr_self = { ppat_loc } } -> mapper ppat_loc) ;
    class_type              = (fun _ { pcty_loc } -> mapper pcty_loc) ;
    class_type_declaration  = (fun _ { pci_loc } -> mapper pci_loc) ;
    class_type_field        = (fun _ { pctf_loc } -> mapper pctf_loc) ;
    constructor_declaration = (fun _ { pcd_loc } -> mapper pcd_loc) ;
    expr                    = (fun _ { pexp_loc } -> mapper pexp_loc) ;
    extension               = (fun _ ({ loc }, _) -> mapper loc) ;
    extension_constructor   = (fun _ { pext_loc } -> mapper pext_loc) ;
    include_declaration     = (fun _ { pincl_loc } -> mapper pincl_loc) ;
    include_description     = (fun _ { pincl_loc } -> mapper pincl_loc) ;
    label_declaration       = (fun _ { pld_loc } -> mapper pld_loc) ;
    location                = (fun _ -> mapper) ;
    module_binding          = (fun _ { pmb_loc } -> mapper pmb_loc) ;
    module_declaration      = (fun _ {pmd_loc } -> mapper pmd_loc) ;
    module_expr             = (fun _ { pmod_loc } -> mapper pmod_loc) ;
    module_type             = (fun _ { pmty_loc } -> mapper pmty_loc) ;
    module_type_declaration = (fun _ { pmtd_loc } -> mapper pmtd_loc) ;
    open_description        = (fun _ { popen_loc } -> mapper popen_loc) ;
    pat                     = (fun _ { ppat_loc } -> mapper ppat_loc) ;
    signature               = (fun _ -> function | [] -> [] | { psig_loc } :: _ -> mapper psig_loc) ;
    signature_item          = (fun _ { psig_loc } -> mapper psig_loc) ;
    structure               = (fun _ -> function | [] -> [] | { pstr_loc } :: _ -> mapper pstr_loc) ;
    structure_item          = (fun _ { pstr_loc } -> mapper pstr_loc) ;
    typ                     = (fun _ { ptyp_loc } -> mapper ptyp_loc) ;
    type_declaration        = (fun _ { ptype_loc } -> mapper ptype_loc) ;
    type_extension          = (fun _ { ptyext_path = { loc } } -> mapper loc) ;
    value_binding           = (fun _ { pvb_loc } -> mapper pvb_loc) ;
    value_description       = (fun _ { pval_loc } -> mapper pval_loc) ;
    type_kind               = 
      (fun _ -> 
        function
        | Ptype_abstract                   -> Ptype_abstract
        | Ptype_open                       -> Ptype_open
        | Ptype_variant []                 -> Ptype_variant []
        | Ptype_variant ({ pcd_loc } :: _) -> mapper pcd_loc
        | Ptype_record []                  -> Ptype_record []
        | Ptype_record ({ pld_loc  }:: _)  -> mapper pld_loc) ;
    with_constraint         = 
      (fun _ ->
        function
        | Pwith_type ({ loc }, _) -> mapper loc
        | Pwith_module ({ loc }, _) -> mapper loc
        | Pwith_typesubst { ptype_loc } -> mapper ptype_loc
        | Pwith_modsubst ({ loc }, _) -> mapper loc) ;
    payload                 = 
      (fun _ -> 
        function 
        | PStr [] -> PStr [] 
        | PSig [] -> PSig []
        | PStr ({ pstr_loc } :: _) -> mapper pstr_loc
        | PSig ({ psig_loc } :: _) -> mapper psig_loc
        | PTyp { ptyp_loc }        -> mapper ptyp_loc
        | PPat ( { ppat_loc }, _ ) -> mapper ppat_loc);
  } 