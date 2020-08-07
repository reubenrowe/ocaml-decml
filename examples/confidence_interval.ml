open Containers

open Decml
open   Model
open   Optimise

open Frontend

(* Is this correct, because the gradient descent ends up fitting lower and upper
   to the same line! *)
let%model ci x = 
  let%pc a = 1 in
  let upper = (a *. x) +. [%pc 1] in
  let lower = (a *. x) +. [%pc 0] in
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
    match List.map fst data with
    | [] -> ()
    | x::xs ->
      let (min, max) =
        let rec f (min, max) =
          function
          | [] ->
            (min, max)
          | x::xs ->
            let min = if Float.Infix.(x <. min) then x else min in
            let max = if Float.Infix.(x >. max) then x else max in
            f (min, max) xs in
        f (x, x) xs in
      let min_x = floor min in
      let (min_y_lower, min_y_upper) = model min_x in
      let max_x = ceil max in
      let (max_y_lower, max_y_upper) = model max_x in
      (* let () =
        Format.fprintf Format.std_formatter
          "(%f, %f, %f)@." min_x min_y_lower min_y_upper in
      let () =
        Format.fprintf Format.std_formatter
          "(%f, %f, %f)@." max_x max_y_lower max_y_upper in *)
      let open Gnuplot in
      let open Color in
      let plot = create () in
      let () =
        set ~output:(Output.(create `X11)) plot in
      let () =
        plot_many plot [ 
            (Series.points_xy data ~color:`Blue) ;
            (Series.lines_xy ~color:`Red
              [ (min_x, min_y_lower); (max_x, max_y_lower); ]) ;
            (Series.lines_xy ~color:`Red
              [ (min_x, min_y_upper); (max_x, max_y_upper); ]) ;
          ] in
      Gnuplot.close plot
  