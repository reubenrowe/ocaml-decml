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
let nested_decouple loc =
  Location.raise_errorf ~loc "Decoupling not supported within models!"
let rec_model loc =
  Location.raise_errorf ~loc "Mutual recursion not supported for models!"
let continuation loc =
  Location.raise_errorf ~loc "Expecting a continuation expression!"
let ifthenelse loc = 
  Location.raise_errorf ~loc "Expecting an else branch!"

let unsupported_model loc = 
  Location.raise_errorf ~loc "Not supported in models!"
