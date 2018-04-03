open Containers

open Decml
open   Model
open   Optimise

open Frontend

let%model l1 x = [%pc] *. x +. [%pc]
let%model l2 x = [%pc] *. x +. [%pc]

let data =
  get_data
    (fun xs -> 
      let x = float_of_string (List.nth xs 0) in
      let y = float_of_string (List.nth xs 1) in
      x, y)

let loss_points m =
  List.map (fun d -> mse m [d]) (data)

let model =
  let%decouple (l1, p1) = l1 in
  let%decouple (l2, p2) = l2 in
  let meta_steps = 500 in
  let rec fit ((p1, p2) as params) (d1, d2) step =
    if step >= meta_steps then
      params
    else 
      let epochs = !iterations / meta_steps in
      let p1 = 
        grad_desc ~loss_f:mse ~rate:!rate ~threshold:!threshold ~epochs 
          ~model:l1 p1 d1 in
      let p2 = 
        grad_desc ~loss_f:mse ~rate:!rate ~threshold:!threshold ~epochs 
          ~model:l2 p2 d2 in
      (* The following implements what is in Victor Darvariu's library, but here
         it seems to lead to very lop-sided results: after the first iteration
         all the data ends up in one slice and not the other, so one model gets
         fitted to the desired level, while the other one remains very poorly
         fitted to the data. *)
      (* let losses = 
        List.map2 (-.) 
          (loss_points (rebind l1 p1))
          (loss_points (rebind l2 p2)) in
      let () = prerr_string "Computing new slices: " in
      let slices =
        List.fold_left2 
          (fun (d1, d2) loss d -> 
            let () = prerr_string (Format.sprintf "%.5f " loss) in
            if Float.(loss < 0.0) then (d::d1, d2) else (d1, d::d2))
          ([], [])
          (losses)
          (data) in
      let () = prerr_newline () in
      let slices = Pair.map_same List.rev slices in *)
      (* Just doing a random slicing seems to produce better results. *)
      let slices = List.partition (fun _ -> Random.bool ()) data in
      fit (p1, p2) slices (step + 1) in
  let slices = List.take_drop (List.length data / 2) data in
  let p1, p2 = fit (p1, p2) slices 0 in
  let l1 = Model.Ex (l1, p1) in
  let l2 = Model.Ex (l2, p2) in
  let%decouple (m, p) = [%model fun x -> l1 x, l2 x] in
  rebind m p 

let () =
  output model (Pair.pp ~sep:" " Float.pp Float.pp) !start_x
