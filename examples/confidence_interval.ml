open Containers

open Decml
open   Model
open   Optimise

open Frontend

let%model ci x = 
  let upper = ([%pc 1] *. x) +. [%pc 1] in
  let lower = ([%pc 1] *. x) +. [%pc 0] in
  upper, lower

let data = 
  get_data
    (fun xs ->
      let x = float_of_string (List.nth xs 0) in
      let y = float_of_string (List.nth xs 1) in
      x, y)

let model =
  let%decouple (model, params) = ci in
  let params = 
    grad_desc ~loss_f:confidence_interval 
      ~rate:!rate ~threshold:!threshold ~epochs:!iterations ~model 
      params data in
  rebind model params

let () =
  output model (Pair.pp ~sep:" " Float.pp Float.pp) !start_x
