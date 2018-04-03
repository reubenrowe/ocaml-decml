open Containers
open Decml

let data_file = ref None
let iterations = ref 50000
let threshold = ref 1.0
let rate = ref 0.00001
let start_x = ref 0.0
let end_x = ref 10.0
let step = ref 1.0

let speclist = [
    ("-data-file", Arg.String (fun s -> data_file := Some s), 
      ": file from which to read data samples") ;
    ("-iterations", Arg.Int (fun i -> iterations := i), 
      Format.sprintf 
        ": maximum number of iterations to perform during model optimisation \
           (default = %i)" !iterations) ;
    ("-threshold", Arg.Float (fun f -> threshold := f),
      Format.sprintf
        ": desired error threshold for optimisation (default = %f)"
        !threshold) ;
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
                    [-threshold <float>] \
                    [-learning-rate <float>] \
                    [-start <float>] \
                    [-end <float>] \
                    [-step <float>] \
                    -data-file <file>"

let () = Arg.parse speclist (fun _ -> ()) usage

let get_data extract =
  match !data_file with
  | None ->
    failwith "Must specify a data file!"
  | Some file ->
    Data.of_csv_file (List.map extract) file

let rec output f pp x =
  if Containers.Float.(x <= !end_x) then
    let () = 
      print_string
        (Format.sprintf "@[<h>%f %a@]@." x pp (f x)) in
    output f pp (x +. !step)
    