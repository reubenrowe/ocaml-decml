(** Module encapsulating model parameter vectors. *)
module Parameters : sig

  type 'a t
  (** The type of model parameter vectors. The type parameter acts as a 
      type-level identifier of the particular vector space in which vectors 
      live. *)

  val null : unit t
  (** The (unique) parameter vector in the nullary vector space. This can be 
      used to construct models from directly lifted OCaml values. *)

  val bases : 'a t -> 'a t list
  (** [bases ps] returns a list of parameter vectors that represents a basis set 
      for the vector space in which [ps] lives. *)

  val unit : 'a t -> 'a t
  (** [unit ps] returns the unit vector in the vector space in which [ps] 
      lives. *)

  val is_zero : 'a t -> bool
  (** Tests whether all the parameters in the given vector are zero. *)

  val magnitude : 'a t -> Carrier.t
  (** The magnitude of a parameter vector *)

  val pp : 'a t Containers.Format.printer
  (** A formatter for printing parameter vectors. *)

  (** {0 Vector Space Operations} *)

  (** Module containing infix versions of the vector space operations. *)
  module Infix : sig

    (** {0 Scalar Operations} *)

    val (<+.>) : Carrier.t -> 'a t -> 'a t
    (** Scalar addition to a parameter vector. *)

    val (<-.>) : Carrier.t -> 'a t -> 'a t
    (** Scalar addition to a parameter vector. *)

    val (<*.>) : Carrier.t -> 'a t -> 'a t
    (** Scalar multiplicat of a parameter vector. *)

    (** {0 Vector Operations} *)

    val (<+>) : 'a t -> 'a t -> 'a t
    (** Pointwise addition of two parameter vectors. *)

    val (<->) : 'a t -> 'a t -> 'a t
    (** Pointwise addition of two parameter vectors. *)

    val (<*>) : 'a t -> 'a t -> 'a t
    (** Pointwise multiplication of two parameter vectors. *)

    val (<.>) : 'a t -> 'a t -> Carrier.t
    (** Dot product of two parameter vectors. *)

    val (~<>) : 'a t -> 'a t
    (** Reflection of a parameter vector. *)

  end

  (** {1 Scalar Operations} *)

  val add : Carrier.t -> 'a t -> 'a t
  (** Scalar addition to a parameter vector. *)

  val subtract : Carrier.t -> 'a t -> 'a t
  (** Scalar subtraction from a parameter vector. *)

  val mult : Carrier.t -> 'a t -> 'a t
  (** Scalar multiplication of a parameter vector. *)

  (** {1 Vector Operations} *)

  val plus : 'a t -> 'a t -> 'a t
  (** Pointwise addition of two parameter vectors. *)

  val sum : 'a t list -> 'a t Option.t
  (** Sum of a list of parameter vectors. Returns None if the input list is
      empty. *)

  val minus : 'a t -> 'a t -> 'a t
  (** Pointwise subtraction of two parameter vectors. *)

  val times : 'a t -> 'a t -> 'a t
  (** Pointwise multiplication of two parameter vectors. *)

  val product : 'a t list -> 'a t Option.t
  (** Product of a list of parameter vectors. Returns None if the input list is
      empty. *)

  val dot : 'a t -> 'a t -> Carrier.t
  (** Dot product of two parameter vectors. *)

  val refl : 'a t -> 'a t
  (** Reflection of a parameter vector: [refl ps] is equivalent to
      [mult (-1.0) ps]. *)

end

type ('a, 'b) perm =
  | Weak : ('b, 'a * 'b) perm
  | Exch : ('a * ('b * 'c), 'b * ('a * 'c)) perm
  | Cong : ('a, 'b) perm -> (('c * 'a), ('c * 'b)) perm
(** Represents permutations and weakenings of typing contexts. *)

type ('ctxt, +'res, 'space) boxed
(** The type of parameterised functions created internally by the module. *)

type ('ctxt, +'res, 'space) parameterised =
  | Boxed  : ('a, 'b, 'c) boxed -> ('a, 'b, 'c) parameterised
  | Lifted : 'b -> (unit, 'b, unit) parameterised
(** The type of parameterised functions. These are either boxed (created by the
    combinators in the module), or directly lifted OCaml values.  *)

type ('a, +'b) t = 
  Ex : ('a, 'b, 'c) parameterised * 'c Parameters.t -> ('a, 'b) t
(** The type of models - an explicit pairing of a parameterised function with 
    its parameter vector. *)

val rebind : 
  (unit, 'b, 'c) parameterised -> 'c Parameters.t -> 'b
(** Rebinds a parameterised function with an appropriate parameter vector and
    evaluates it. *)

val rebind_open : 
  ('a, 'b, 'c) parameterised -> 'c Parameters.t -> 'a -> 'b
(** Rebinds an open parameterised function (i.e. containing free variables) with
    an appropriate parameter vector and produces an evaluation function for it,
    which requires an environment with values for the free variables. *)

val dimension : ('a, 'b) t -> int
(** Returns the dimension of a model (i.e. how many parameters it has). *)

(** {0 Configuration} *)

val default_constant : Carrier.t -> unit
(** Sets the default initial value to be used for provisional constants. *)
   
(** {0 Combinators for building models} *)

(** {1 Basic Abductive Calculus} *)

val pc : ?init:string -> unit -> (unit, Carrier.t) t
(** Return a model consisting of a single fresh provisional constant. *)

val pv : int -> (unit, Carrier.t list) t
(** [pv n] returns a model consisting of a vector of length [n] of fresh
    provisional constants. *)

val var : (('a * unit), 'a) t
(** Return a model consisting of a single free variable. *)

val app : ('c, 'a -> 'b) t -> ('c, 'a) t -> ('c, 'b) t
(** Function application. *)

val abs : ('a * 'c, 'b) t -> ('c, 'a -> 'b) t
(** Function abstraction. *)

val let_bind : ('c, 'a) t -> ('a * 'c, 'b) t -> ('c, 'b) t
(** Let binding. *)

val tx : ('a, 'b) perm -> ('a, 'c) t -> ('b, 'c) t
(** Structural transformation of the typing context. *)

(** {1 Simple Extensions} *)

val unit : (unit, unit) t
(** The lifted unit value. *)

val _true : (unit, bool) t
(** The lifted [true] boolean value. *)

val _false : (unit, bool) t
(** The lifted [false] boolean value. *)

val pair : ('c, 'a) t * ('c, 'b) t -> ('c, 'a * 'b) t
(** Pair construction. *)

val ifelse : ('a, bool) t * ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
(** If-then-else construction. *)

val abs_rec : (('a * (('a -> 'b) * 'c)), 'b) t -> ('c, 'a -> 'b) t
(** Abstraction of a recursive function, where the argument is the top variable
    in the context, and the recursive instance variable is the second one. *)

(** {1 Meta Functions} *)

val fmap : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
(** Functorial lifting of arrows. *)
