open Containers.List
open Containers.Fun

open Model
open   Parameters
open     Infix

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
      (fun _ d ->
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

let confidence_interval f data =
  let ds = 
    List.filter 
      (fun (x, y) -> 
        let z, z' = fst (f x), snd (f x) in
        (z < y && z' < y) || (z > y && z' > y))
      (data) in
  let avg_mse = (mse (f %> fst) ds +. mse (f %> snd) ds) /. 2.0 in
  let line_dist =
    let (x, y) = f (fst (List.hd data)) in
    sqrt (abs_float ((x *. x) -. (y *. y))) in
  avg_mse +. (line_dist *. line_dist)
  
let grad_desc ~loss_f ~rate ~threshold ~epochs ~model params data =
  let rec grad_desc current_epoch params =
    let loss = loss_f (model params) data in
    if current_epoch >= epochs || Containers.Float.(loss <= threshold)  then
      let () = 
        Format.fprintf Format.err_formatter
          "Completed optimisation after %i iterations with error %.5f@."
          current_epoch loss in
      params
    else
      (* let () = 
        Format.fprintf Format.err_formatter 
          "@[<h>Step %d. Current loss: %.10f. %a@]@." 
          epochs (loss_f (model params) data) Parameters.pp params in *)
      let dfs =  (* Partial derivatives at x for each basis of the vector space *)
        let x = random_choose data (Random.get_state ()) in
        let f ps = loss_f (model ps) [x] in
        map 
          (fun b -> 
            let df = 
              let b = !dx <*.> b in
              ((f (params <+> b) -. f (params <-> b)) /. (2.0 *. !dx)) in
            -.(rate *. df) <*.> b)
          (bases params) in
    let params = fold_left plus params dfs in
    grad_desc (current_epoch + 1) params in
  grad_desc 0 params

let grad_desc ~loss_f ~rate ~threshold ~epochs ~model params data =
  let model = rebind model in
  grad_desc ~loss_f ~rate ~threshold ~epochs ~model params data
