(** A module containing lifted versions of values from the OCaml standard 
    library. This can be opened to provide an overlay. *)

open Model

module Pervasives : sig
  val ( +  ) : (unit, int -> int -> int) t
  val ( -  ) : (unit, int -> int -> int) t
  val ( *  ) : (unit, int -> int -> int) t
  val ( /  ) : (unit, int -> int -> int) t
  val ( +. ) : (unit, float -> float -> float) t
  val ( -. ) : (unit, float -> float -> float) t
  val ( *. ) : (unit, float -> float -> float) t
  val ( /. ) : (unit, float -> float -> float) t
  val ( =  ) : (unit, 'a -> 'a -> bool) t
  val ( <  ) : (unit, 'a -> 'a -> bool) t
  val ( <= ) : (unit, 'a -> 'a -> bool) t
  val ( >  ) : (unit, 'a -> 'a -> bool) t
  val ( >= ) : (unit, 'a -> 'a -> bool) t
  val ( || ) : (unit, bool -> bool -> bool) t
  val ( && ) : (unit, bool -> bool -> bool) t
  val fst : (unit, 'a * 'b -> 'a) t
  val snd : (unit, 'a * 'b -> 'b) t
end

(** A module containing lifted versions of values operating on lists *)
module List : sig
  val empty : (unit, 'a list) t
  val map : (unit, ('a -> 'b) -> 'a list -> 'b list) t
end
