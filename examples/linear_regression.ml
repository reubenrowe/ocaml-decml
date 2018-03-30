open Containers

open Decml
open   Model
open   Optimise

let data_file = ref None
let iterations = ref 5000
let rate = ref 0.00001
let start_x = ref 0.0
let end_x = ref 10.0
let step = ref 1.0

let speclist = [
    ("-data-file", Arg.String (fun s -> data_file := Some s), 
      ": file from which to read data samples") ;
    ("-iterations", Arg.Int (fun i -> iterations := i), 
      Format.sprintf 
        ": number of iterations to perform during model optimisation \
           (default = %i)" !iterations) ;
    ("-learning-rate", Arg.Float (fun f -> rate := f), 
      Format.sprintf ": learning rate (default %f)" !rate) ;
    ("-start", Arg.Float (fun f -> start_x := f), 
      Format.sprintf 
        ": initial [x] value for output of optimised model (default = %f)" 
        !start_x) ;
    ("-end", Arg.Float (fun f -> end_x := f), 
      Format.sprintf 
        ": final [x] value for output of optimised model (default = %f)" !end_x) ;
    ("-step", Arg.Float (fun f -> step := f), 
      Format.sprintf 
        ": [x] step value for output of optimised model (default = %f)" !step) ;
  ]

let usage = "usage: [-iterations <int>] \
                    [-learning-rate <float>] \
                    [-start <float>] \
                    [-end <float>] \
                    [-step <float>] \
                    -data-file <file>"

let () = Arg.parse speclist (fun _ -> ()) usage

let data =
  match !data_file with
  | None ->
    failwith "Must specify a data file!"
  | Some f ->
    let load =
      Data.of_csv_file
        (List.map 
          (fun xs -> 
            let x = float_of_string (List.nth xs 0) in
            let y = float_of_string (List.nth xs 1) in
            x, y)) in
    load f

(* model m x = 1.0[@pc] * x + 0.0[@pc] *)
let model =
  let open Overlay.Pervasives in
  abs 
    (app
      (app 
        (tx Weak (+.)) 
        (app 
          (app 
            (tx Weak ( *. )) 
            (tx Weak (pc ~init:"1.0" ()))) 
          (var))) 
      (tx Weak (pc ~init:"0.0" ())))

let model =
  let Ex (m, p) = model in
  let m = rebind m in
  let p = 
    grad_desc ~loss_f:mse ~rate:!rate ~epochs:!iterations ~model:m p data in
  m p

let rec output f x =
  if Float.compare x !end_x <= 0 then
    let () = 
      print_endline
        (Format.sprintf "x = %f, y = %f" x (f x)) in
    output f (x +. !step)

let () =
  output model !start_x
