open Containers.List

open Model
open   Parameters

type ('a, 'b) loss_function = ('a -> 'b) -> 'a Data.t -> Carrier.t

let dx = ref 0.00001

let set_dx n = 
  if Containers.Float.compare n 0.0 <= 0
    then invalid_arg (Format.sprintf "%s.set_dx" __MODULE__)
    else dx := n

let mse f data = 
  let sum = 
    fold_left
      (fun acc d -> let e = (snd d) -. f (fst d) in acc +. (e *. e))
      (0.0)
      (data) in
  if Containers.Float.equal sum 0.0
    then sum
    else (1.0 /. (2.0 *. (float_of_int (length data)))) *. sum

let cross_entropy f data =
  let sum =
    fold_left
      (fun acc d ->
        let y = snd d in
        let y_hat = 
          match f (fst d) with
          | [] -> 0.0
          | x::xs -> fold_left Containers.Float.max x xs in
        (y *. log y_hat) +. ((1.0 -. y) *. log (1.0 -. y_hat)))
      (0.0)
      (data) in
  if Containers.Float.equal sum 0.0
    then sum
    else -. (sum /. (float_of_int (length data)))
  
let rec grad_desc ~loss_f ~rate ~epochs ~model params data =
  if epochs = 0 then
    params
  else
    let x = random_choose data (Random.get_state ()) in
    let dfs =  (* Partial derivatives for each basis of the vector space *)
      let f ps = loss_f (model ps) [x] in
      map 
        (fun b -> 
          let df = 
            ((f (plus params b) -. f (plus params (refl b))) /. 0.00002) in
          mult (rate *. df) (refl b))
        (map (mult !dx) (bases params)) in
   let params = fold_left plus params dfs in
   let epochs = epochs - 1 in
   grad_desc ~loss_f ~rate ~epochs ~model params data
