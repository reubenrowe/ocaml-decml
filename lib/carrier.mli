(** The carrier domain that models operator over, and in which parameters 
    live. *)

type t = float
(* Type of the carrier domain. *)

val of_string : string -> t
(** Parse a carrier value from a string. Raises Invalid_argument if the string
    does not represent a valid carrier value. *)
