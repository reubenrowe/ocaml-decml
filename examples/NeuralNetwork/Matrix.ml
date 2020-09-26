module type Matrix = sig
  type t 
  val dot_product : t -> t -> float
  val rand_structure : int -> t
  val length_of : t -> int
  val difference : t -> t -> t
  val struct_min : t -> float
  val struct_max : t -> float
  val data : string -> t list
  val labels : string -> t list
  val train_split : int -> t list -> t list
  val test_split : int -> t list -> t list
  val normalize_struct : t -> t
  val array_to_vec : float array -> t
  val test_errors : t list -> unit

  val add : t -> t -> t
  val get_head : t -> float   

(* end

module type Matrix = sig *)
  type m 

  val mat_diff: m -> m -> m
  val length: m -> int
  val rand_mat : int -> int -> m
  val transpose : m -> m
  val print_head : m -> unit
  val matrix_multiply : m -> m -> m
  val multiply_map : t -> m -> t -> t
  val hadamard : m -> m -> m
  val make : t -> m
  val mse : t -> t-> float
  

  val array_to_mat : float array array -> m

  type n 
  val weights_gen : int list -> m list
  val bias_gen : int list -> t list
  val feed_forward : t -> m list -> t list -> t
  val output_accumulator : t -> m list -> t list -> t list
  val compute_errors : t -> t -> m list -> t list -> t list
  val update_weights : float -> t list -> t list -> m list -> m list
  val update_biases : float -> t list -> t list -> t list
 end 
 (* module type Network = sig
  open M
  val train : float -> t list -> m list -> t list -> t list -> m list * t list
  val test : float -> t list -> m list -> t list -> t list -> int
 end *)