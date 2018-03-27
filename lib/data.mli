(** A module ecapsulating sampled data used for optimising models. *)

type 'a t = ('a * Carrier.t) list
(** Type for function sample data. *)

val of_csv_file : 
  ?separator:char -> ?strip:bool -> ?backslash_escape:bool
    -> ?excel_tricks:bool -> ?fix:bool -> ?header:bool 
      -> (Csv.t -> 'a t) -> string -> 'a t