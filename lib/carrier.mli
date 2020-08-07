(** The carrier domain that models operator over, and in which parameters 
    live. *)

type t = float
(* Type of the carrier domain. *)

val ( = ) : t -> t -> bool
(** Equality of carrier values. *)

val ( < ) : t -> t -> bool
(** Strict inequality (less) of carrier values. *)

val ( <= ) : t -> t -> bool
(** Inequality (less) of carrier values. *)

val ( > ) : t -> t -> bool
(** Strict inequality (greater) of carrier values. *)

val ( >= ) : t -> t -> bool
(** Inequality (greater) of carrier values. *)

val ( <> ) : t -> t -> bool
(** Disequality of carrier values. *)

val ( + ) : t -> t -> t
(** Addition of carrier values. *)

val ( - ) : t -> t -> t
(** Subtraction of carrier values. *)

val ( * ) : t -> t -> t
(** Multiplication of carrier values. *)

val ( / ) : t -> t -> t
(** Division of carrier values. *)

val abs : t -> t
(** Absolute magnitude of carrier values. *)

val zero : t
(** The zero value *)

val pp : t Containers.Format.printer
(** A formatter for printing carrier values. *)

val of_string : string -> t
(** Parse a carrier value from a string. Raises Invalid_argument if the string
    does not represent a valid carrier value. *)
