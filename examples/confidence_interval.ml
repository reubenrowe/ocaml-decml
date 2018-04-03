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
    (List.map 
      (fun xs -> 
        let x = float_of_string (List.nth xs 0) in
        let y = float_of_string (List.nth xs 1) in
        x, y))

let model =
  let%decouple (m, p) = ci in
  let m = rebind m in
  let p = 
    grad_desc ~loss_f:confidence_interval 
      ~rate:!rate ~threshold:!threshold ~epochs:!iterations 
      ~model:m p data in
  m p

let () =
  output model (Pair.pp ~sep:" " Float.pp Float.pp) !start_x
