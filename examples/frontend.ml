open Containers
open Decml

let data_file = ref None
let iterations = ref 50000
let threshold = ref 1.0
let rate = ref 0.00001
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
  ]

let usage = "usage: [-iterations <int>] \
                    [-threshold <float>] \
                    [-learning-rate <float>] \
                    -data-file <file>"

let () = Arg.parse speclist (fun _ -> ()) usage

let get_data extract =
  match !data_file with
  | None ->
    failwith "Must specify a data file!"
  | Some file ->
    Data.of_csv_file (List.map extract) file
