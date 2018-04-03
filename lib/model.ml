open Containers

module Key = struct

  type t = int

  let equal k k' = k = k'
  let compare k k' = compare k k'

  let current = ref 0

  let get_fresh () =
    let cur = !current in
    if cur < max_int then
      let () = current := cur + 1 in
      cur
    else
      failwith "Cannot generate a fresh parameter ID!"

end

module Map = Map.Make(Key)

module Parameters = struct

  type 'a t = 
    P : Carrier.t Map.t -> 'a t

  let pp fmt (P ps) =
    (List.pp Carrier.pp) fmt (List.map snd (Map.bindings ps))

  let null = P Map.empty

  let merge (P p) (P p') =
    P (Map.merge (fun k v v' -> Option.or_ ~else_:v' v) p p')

  let unit (P v) =
    P (Map.map (fun _ -> 1.0) v)

  let bases (P v) =
    List.map 
      (fun (k, _) -> 
        P (Map.mapi (fun k' _ -> if k = k' then 1.0 else 0.0) v)) 
      (Map.bindings v)

  let add x (P v) =
    P (Map.map ((+.) x) v)
  let subtract x (P v) =
    P (Map.map ((-.) x) v)
  let mult x (P v) =
    P (Map.map (( *. ) x) v)
  let refl v =
    mult (-1.0) v

  let combine f v v' =
    let err = 
      lazy 
        (failwith
          (Format.sprintf "%s: incompatible parameter spaces!" __MODULE__)) in
    try
      let bindings =
        List.map2
          (fun b b' ->
            match b, b' with
            | (k, v), (k', v') when k = k' -> 
              k, f v v'
            | _ ->
              Lazy.force err)
          (Map.bindings v)
          (Map.bindings v') in
      Map.of_list bindings
    with Invalid_argument _ ->
      Lazy.force err
  let plus (P v) (P v') =
    P (combine (+.) v v')
  let minus (P v) (P v') =
    P (combine (-.) v v')
  let times (P v) (P v') =
    P (combine ( *. ) v v')
  let dot (P v) (P v') =
    Map.fold (fun _ -> (+.)) (combine ( *. ) v v') 0.0
    
  module Infix = struct
    let (<+.>) v v' = add v v'
    let (<-.>) v v' = subtract v v'
    let (<*.>) v v' = mult v v'
    let (<+>) v v' = plus v v'
    let (<->) v v' = minus v v'
    let (<*>) v v' = times v v'
    let (<.>) v v' = dot v v'
    let (~<>) v = refl v
  end
    
end
(** Module encapsulating model parameter vectors. *)

type ('a, 'b) perm =
  | Weak : ('b, 'a * 'b) perm
  | Exch : ('a * ('b * 'c), 'b * ('a * 'c)) perm
  | Cong : ('a, 'b) perm -> (('c * 'a), ('c * 'b)) perm
(** Represents permutations and weakenings of typing contexts. *)

type ('a, +'b, 'c) boxed =
  | M : (Carrier.t Map.t * 'a -> 'b) -> ('a, 'b, 'c) boxed
(** The type of parameterised models *)

type ('ctxt, +'res, 'space) parameterised =
  | Boxed  : ('a, 'b, 'c) boxed -> ('a, 'b, 'c) parameterised
  | Lifted : 'b -> (unit, 'b, unit) parameterised

type ('a, +'b) t =
  Ex : ('a, 'b, 'c) parameterised * 'c Parameters.t -> ('a, 'b) t
(** The type of models - an explicit pairing of a parameterised model with its 
    parameter vector. *)

let rebind (type b c) (m : (unit, b, c) parameterised) (Parameters.P params) = 
  match m with
  | Boxed M model -> model (params, ())
  | Lifted f -> f
let rebind_open 
    (type a b c) (m : (a, b, c) parameterised) (Parameters.P params) =
  match m with
  | Boxed M model -> fun ctxt -> model (params, ctxt)
  | Lifted f -> fun () -> f

(* Combinators for building models *)

let pc ?(init="0.0") () =
  let key = Key.get_fresh () in
  let model = fun (params, ()) -> Option.get_exn (Map.get key params) in
  let init = Carrier.of_string init in
  let params = Map.singleton key init in
  Ex (Boxed (M model), Parameters.P params)

let var = Ex (Boxed (M (fun (_, (x, ())) -> x)), Parameters.P Map.empty)

let app (type a b c) (m : (c, a -> b) t) (m' : (c, a) t) =
  let Ex (m, p) = m in
  let Ex (m', p') = m' in
  let ps = Parameters.merge p p' in
  match m, m' with
  | Lifted f, Lifted x ->
    Ex (Boxed (M (fun _ -> f x)), ps)
  | Lifted f, Boxed (M m) ->
    Ex (Boxed (M (fun (input : Carrier.t Map.t * c) -> f (m input))), ps)
  | Boxed M m, Lifted x ->
    Ex (Boxed (M (fun input -> (m input) x)), ps)
  | Boxed M m, Boxed M m' ->
    Ex (Boxed (M (fun input -> (m input) (m' input))), ps)

let abs (Ex (Boxed (M model), params)) =
  let model (params, ctxt) x = model (params, (x, ctxt)) in
  Ex (Boxed (M model), params)

let let_bind m m' =
  app (abs m') m

let rec permute : type a b . (a, b) perm -> b -> a =
  fun (type a b) (perm : (a, b) perm) (ctxt : b) ->
  match perm, ctxt with
  | Weak, (_, ctxt) -> 
    ctxt
  | Exch, (x, (y, ctxt)) -> 
    (y, (x, ctxt))
  | (Cong perm), (x, ctxt) ->
    (x, permute perm ctxt)

let tx (type a b) (perm : (a, b) perm) ((Ex (model, params)) : (a, 'c) t) =
  match model with
  | Lifted f ->
    Ex (Boxed (M (fun _ -> f)), params)
  | Boxed M model ->
    let model (params, ctxt) = model (params, (permute perm ctxt)) in
    Ex (Boxed (M model), params)

let pair (type a b c) ((m : (c, a) t), (m' : (c, b) t)) =
  let Ex (m, p) = m in
  let Ex (m', p') = m' in
  let ps = Parameters.merge p p' in
  match m, m' with 
  | Lifted x, Lifted y ->
    Ex (Boxed (M (fun _ -> x, y)), ps)
  | Lifted x, Boxed (M y) ->
    Ex (Boxed (M (fun (input : _ * c) -> x, (y input))), ps)
  | Boxed (M x), Lifted y ->
    Ex (Boxed (M (fun input -> (x input), y)), ps)
  | Boxed (M x), Boxed (M y) ->
    Ex (Boxed (M (fun input -> (x input), (y input))), ps)
  
let unit = Ex (Boxed (M (fun (_, ()) -> ())), Parameters.P Map.empty)
let _true = Ex (Boxed (M (fun (_, ()) -> true)), Parameters.P Map.empty)
let _false = Ex (Boxed (M (fun (_, ()) -> false)), Parameters.P Map.empty)

let ifelse (type a b) 
    ((b : (a, bool) t), (_if : (a, b) t), (_else : (a, b) t)) =
  let Ex (b, p1), Ex (_if, p2), Ex (_else, p3) = b, _if, _else in
  let b = match b with
    | Lifted b -> (fun _ -> b)
    | Boxed (M b) -> b in
  let _if = match _if with
    | Lifted _if -> (fun _ -> _if)
    | Boxed (M _if) -> _if in
  let _else = match _else with
    | Lifted _else -> (fun _ -> _else)
    | Boxed (M _else) -> _else in
  let model input =
    if (b input) then (_if input) else (_else input) in
  Ex (Boxed (M model), Parameters.(merge p1 (merge p2 p3)))

let abs_rec (type a b c) (m : ((a * ((a -> b) * c)), b) t) =
  let rec fixpoint f = f (fixpoint f) in
  match abs (abs m) with
  | Ex (Lifted _, _) ->
    (* abs does not create a Lifted model *)
    assert false
  | Ex (Boxed (M m), ps) ->
    let m input = fixpoint (m input) in
    Ex (Boxed (M m), ps)
