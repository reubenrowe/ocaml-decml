(** A module representing a parsetree attribute containing data that it is used
    by the PPX rewriter. *)

open Containers

open Migrate_parsetree
open   Ast_404
open     Parsetree

type payload = {
  depth : int ;
}
(** A record type of the fields contained in the attribute. *)

val default : payload
(** A default payload for the attribute. *)

val get : expression -> payload
(** Extract the PPX attribute from an expression. Raises [Not_found] if the
    expression does not have the attribute attached; raises [Failure] if the
    attribute is found, but it malformed. Note that if there are multiple
    Decml PPX attributes attached to the expression then the first one is
    returned, and if the first one is malformed, then a [Failure] exception will
    be raised. *)

val remove : expression -> expression
(** Removes the Decml PPX attribute from the expression, if it exists. Note that
    if the expression contains more than one Decml PPX attributes then only the
    first one is removed. *)

val add : payload -> expression -> expression
(** Prepends a Decml PPX attribute with the given payload to the attribute list
    of the expression. *)