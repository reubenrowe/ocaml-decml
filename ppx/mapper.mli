open Migrate_parsetree
open   Driver
open   Versions

val rewriter : OCaml_404.types rewriter
(** A rewriter for eliminating DecML extensions. *)

val model_rewriter : OCaml_404.types rewriter
(** A rewriter for producing combinatorial expressions evaluating to a model of
    the expression being rewritten. This rewriter is used internally by 
    [rewriter] to rewrite expressions within DecML extensions. *)
