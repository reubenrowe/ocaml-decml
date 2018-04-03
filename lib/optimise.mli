type ('a, 'b) loss_function = ('a -> 'b) -> 'a Data.t -> Carrier.t

val mse : ('a, Carrier.t) loss_function
(** Mean squared error loss function. *)

val cross_entropy : ('a, Carrier.t list) loss_function
(** Cross entropy loss function. *)

val confidence_interval : ('a, Carrier.t * Carrier.t) loss_function
(** A confidence interval loss function. *)

val set_dx : float -> unit
(** Sets the dx value used by the optimisation functions for computing 
    partial derivatives. *)

val grad_desc : 
  loss_f:(('a, 'b) loss_function) -> rate:float -> threshold:float -> epochs:int
    -> model:(unit, 'a -> 'b, 'c) Model.parameterised
      -> 'c Model.Parameters.t -> 'a Data.t -> 'c Model.Parameters.t
(** Gradient descent optimisation function. *)