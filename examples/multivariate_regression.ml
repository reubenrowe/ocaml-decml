open Containers
open   Fun
open   List
open   Pair


open Decml
open   Model
open   Optimise

open Frontend

let data =
  get_data
    ((List.rev_map float_of_string)
      %> hd_tl
      %> map2 (fun x -> 1.0 :: x)
      %> swap)

let rec mk_vector n =
  if n < 0 then
    invalid_arg (Format.sprintf "%s.mk_vector" __MODULE__)
  else if n = 0 then
    [%model [] ]
  else
    (* fmap (fun (hd, tl) -> hd :: tl) (pair ([%pc 1], (mk_vector (n-1)))) *)
    app (fmap List.cons [%pc 1]) (mk_vector (n-1))


let mvr =
  let%lift dot = List.fold_left2 (fun dp x y -> dp +. (x *. y)) 0.0 in
  let w = 
    (* We get very different results depending on whether we use Model.pv or
       the mk_vector function defined above - why is this? *)
    pv 14 in
  [%model fun x -> dot w x ]

let model =
  let%decouple (model, params) = mvr in
  let params = 
    grad_desc ~loss_f:mse ~rate:!rate ~threshold:!threshold ~epochs:!iterations 
      ~model params data in
  rebind model params

(* let () =
  output model Float.pp !start_x *)
  