open Containers

open Decml
open   Model
open   Optimise

open Frontend

let%model linear x = ([%pc 1] *. x) +. [%pc 0]

let data =
  get_data
    (fun xs -> 
      let x = float_of_string (List.nth xs 0) in
      let y = float_of_string (List.nth xs 1) in
      x, y)

let model =
  let%decouple (model, params) = linear in
  let params = 
    grad_desc ~loss_f:mse ~rate:!rate ~threshold:!threshold ~epochs:!iterations 
      ~model params data in
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
    let min = floor min in
    let max = ceil max in
    let open Gnuplot in
    let open Color in
    let plot = create () in
    let () =
      set ~output:(Output.(create `X11)) plot in
    let () =
      plot_many plot [ 
          (Series.points_xy data ~color:`Blue) ;
          (Series.lines_xy ~color:`Red
            [ (min, model min); (max, model max); ]) ;
        ] in
    Gnuplot.close plot
