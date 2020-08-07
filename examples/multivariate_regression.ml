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
  
let (training_data, test_data) =
  take_drop (((length data) / 10) * 9) data

let rec mk_vector n =
  if n < 0 then
    invalid_arg (Format.sprintf "%s.mk_vector" __MODULE__)
  else if n = 0 then
    [%model [] ]
  else
    (* fmap (fun (hd, tl) -> hd :: tl) (pair ([%pc 0], (mk_vector (n-1)))) *)
    app (fmap List.cons [%pc 0]) (mk_vector (n-1))

let mvr =
  let%lift dot = List.fold_left2 (fun dp x y -> dp +. (x *. y)) 0.0 in
  let w = pv 14 in
  [%model fun x -> dot w x ]

let model =
  let%decouple (model, params) = mvr in
  let params = 
    grad_desc ~loss_f:mse ~rate:!rate ~threshold:!threshold ~epochs:!iterations 
      ~model params training_data in
  rebind model params

let () =
  let output_data_point (input, expected) =
    let predicted = model input in
    print_endline
      (Format.sprintf
        "@[<h>data point: (%a)@.\texpected: %f, predicted: %f@]"
          (Format.list Format.pp_print_float) input
          expected
          predicted)
    in
  List.iter output_data_point test_data
  