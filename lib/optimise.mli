type ('a, 'b) loss_function = ('a -> 'b) -> 'a Data.t -> Carrier.t

val mse : ('a, Carrier.t) loss_function

val cross_entropy : ('a, Carrier.t list) loss_function

val set_dx : float -> unit

val grad_desc : 
  loss_f:(('a, 'b) loss_function) -> rate:float -> threshold:float -> epochs:int
    -> model:('c Model.Parameters.t -> 'a -> 'b)
      -> 'c Model.Parameters.t -> 'a Data.t -> 'c Model.Parameters.t