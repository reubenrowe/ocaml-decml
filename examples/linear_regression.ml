open Containers

open Decml
open   Model
open   Optimise

open Frontend

let%model linear x = [%pc 1.0] *. x +. [%pc 0.0]

let model =
  let%decouple (m, p) = linear in
  let m = rebind m in
  let p = 
    grad_desc ~loss_f:mse ~rate:!rate ~threshold:!threshold ~epochs:!iterations 
      ~model:m p data in
  m p

let () =
  output model !start_x
