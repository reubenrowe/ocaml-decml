
open Csv
open Unix

(*The following code is an absolute hairball. Read at your own peril*)


(*Generate a random float*)

(*Generate the weights from an int list*)
let rec weights_gen (list:int list) =
    match list with
    [] -> []
    |_::[] -> []
    |first::second::lst ->
      let weight_mat = ArrayUtils.rand_flt_matrix first second in
      weight_mat::(weights_gen (second::lst))
;;
(*Generate the bias from an int list*)
let rec bias_gen (list:int list) = 
    match list with
    [] -> []
    | _::[] -> []
    |first::second::lst ->
      let bias_vec = Array.make second 0. in
      bias_vec::(bias_gen(second::lst))
;;
(*Sigmoid function*)
let sigmoid_elem elem =
 1. /. (1. +. (exp (-. 1. *. elem)))
;;
(*Compute feed forward for a single neuron*)
let feed_forward_neuron data weights bias =
  let alpha' = ArrayUtils.dot_product data weights +. bias in
    sigmoid_elem alpha'
;;
(*Compute feed forward for the next layer*)
let feed_forward_layer data weights bias =
   Array.map2 (feed_forward_neuron data) weights bias 
;;
(*Gives the output of a network*)
let feed_forward_network data weights bias  =
  List.fold_left2 feed_forward_layer data weights bias 
;;
(*Accumulate the outputs of each layer for the feed forward process*)
let rec output_accumulator data weights bias = 
  let weights_head = List.hd weights in
  let bias_head = List.hd bias in

    if List.length weights <> List.length bias then
    invalid_arg "Different list lengths";
  match weights, bias with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths"
  | [], h::t -> invalid_arg "Different list lengths"
  | h::[], h1::[] -> (feed_forward_layer data h h1)::[]
  | h::t, h1::t1 -> 
  let data_1 = feed_forward_layer data weights_head bias_head in
    print_string "Moving to a new layer....";
    print_string "\n";
   (data_1)::output_accumulator data_1 t t1
;; 
(*Other accumulator function. You need the input of the first layer as an "output" for backprop*)
let output_accumulator_2 data weights bias = 
  let acc = output_accumulator data weights bias in
  data::acc
;;











(*Collection of error accumulation functions. I rely on composition because I don't know how to code in FP langauges :)   *)
let error_accumulator_0 (ff_results:float array list)
 (weights:float array array list)
  (label:float array) =
    let network_output = List.hd (ff_results) in
     let first_error = Array.map2 (-.) network_output label in
  first_error::[]
;;
let error_accumulator_1 (ff_result:float array)
(weights:float array array)
(error:float array ) = 
  let stretched_error = Array.make 1 error in
  let product_t = ArrayUtils.matrix_multiply stretched_error weights in 
  let input = Array.map (fun f -> 1. -. f) ff_result in 
  let stretched_input = Array.make 1 input in 
  let stretched_ff_result = Array.make 1 ff_result in
  let output_der = ArrayUtils.elem_wise_mat_mult stretched_ff_result stretched_input in
  (ArrayUtils.elem_wise_mat_mult product_t output_der).(0)
;;
let rec error_accumulator_2 (ff_results:float array list)
(weights:float array array list)
(error:float array) =
  match ff_results, weights with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths (In error_acc)"
  | [], h::t -> []
  | h::t, h1::t1 -> let error_res = error_accumulator_1 h h1 error in
    (error_res)::error_accumulator_2 t t1 error_res
;;
let error_accumulator_4 (ff_results:float array list)
(weights:float array array list)
(error:float array) =
  let ff_results_t = List.tl ff_results in 
  error_accumulator_2 ff_results_t weights error
;;
let error_accumulator_3 (ff_results:float array list)
(weights:float array array list)
(label:float array) = 
  let reversed_ff = List.rev ff_results in
  let reversed_weights = List.rev weights in
  let first_err = List.hd (error_accumulator_0 reversed_ff reversed_weights label) in
  let err_list = error_accumulator_4 reversed_ff reversed_weights first_err in
  first_err::(err_list)
;;


(*Next, compute the change matrices requred to update the weights*)
let weight_delta (learning_rate: float) (error: float array) (output:float array) = 
  let stretched_output = Array.make 1 output in
  let stretched_error = Array.make 1 error in
  let output_t = ArrayUtils.transpose stretched_output in 
    let returner = ArrayUtils.matrix_multiply output_t stretched_error in
    let returner_1 = (Array.make_matrix (Array.length returner) (Array.length returner.(0)) learning_rate) in
  ArrayUtils.transpose (ArrayUtils.elem_wise_mat_mult returner returner_1)
;;
let rec weight_updater_1 (learning_rate: float) (errors: float array list) (outputs: float array list) = 
  match errors, outputs with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths in weight_updater"
  | [], h::t  -> invalid_arg "Different list lengths in weight_updater"
  | h::t, h1::t1 -> let delta_i = weight_delta learning_rate h h1 in
  (delta_i):: weight_updater_1 learning_rate t t1
;;
let weight_updater_2 (learning_rate:float) (errors:float array list) (outputs: float array list) = 
  let outputs_t = List.tl (List.rev outputs) in
  weight_updater_1 learning_rate errors outputs_t
;;

let weight_updater_3 (learning_rate:float) (errors:float array list) (outputs:float array list) =
  let errors_t = List.rev (List.tl (List.rev errors)) in
  let first = weight_updater_2 learning_rate errors_t outputs in 
  List.rev first
;;

let bias_updater (errors:float array list) =
  List.tl (List.rev errors)
;;
(*Execute the above in order to update the weights*)
let update_weights (initial_w:float array array list) (deltas:float array array list) =
  List.map2 ArrayUtils.mat_sum initial_w deltas
;;
(*Find a way to test all of it...*)
let rec train learning_rate data (weights:float array array list) (bias:float array list) labels  = 
  match data, labels with
  | [], [] -> weights::[(Array.of_list bias)]::[]
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
                  let ff_results = output_accumulator_2 h weights bias in
                  let output = feed_forward_network h weights bias in
                  let errors = error_accumulator_3 ff_results weights h1 in 
                  let weight_deltas = weight_updater_3 learning_rate errors ff_results in 
                  let new_weights = update_weights weights weight_deltas in
                  let new_biases = bias_updater errors in
                  print_string "The output at this iteration is:   ";
                  print_float (output.(0));
                  print_string "\n";
                  print_string "The linear difference of this iteration is:  ";
                  print_float((output.(0)) -. (h1.(0)));
                  print_string "\n";
                  train learning_rate t new_weights new_biases t1  
;;

let rec test data weights biases labels =
  match data, labels with
  | [], [] -> 0
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
    let output = feed_forward_network h weights biases in 
    let () = 
      print_string "Expected output: "; ArrayUtils.print_arr h1; print_string "\n";  in
    let () = 
      print_string "Observed output: "; ArrayUtils.print_arr output; print_string "\n" in
    if ((ArrayUtils.mse output h1) > 0.05) then 0 + test t weights biases t1
    else 1 + test t weights biases t1
;;

let rec split_at_point l n =
  if n = 0 then
      l
  else
      match l with
      | [] -> []
      | head :: tail ->
          split_at_point tail (n - 1)
;;

let decoupled_w trained = 
  List.hd trained
;;
let decoupled_b trained =
  List.hd(List.hd(List.rev trained))
;;



  
(*TESTING CODE....*)
let head_dimension list = 
  match list with
    [] -> 0
    | h::t -> Array.length h
;;

let arr_mat list_mat = 
  Array.of_list (List.map Array.of_list list_mat)
;;


let data_init_1 = List.tl (Csv.load "wine_dataset.csv")
;;
let data_post = ArrayUtils.transpose (ArrayUtils.float_mat (arr_mat data_init_1))
;;
let normalized_data = Array.map (ArrayUtils.normalize_arr) data_post
;;
let normalized_labels_i = normalized_data.(11)
;;
let labels = of_array normalized_labels_i
;;
let inputs_func (mat:float array array) = 
  let returner = Array.make_matrix (11) (1599) 0. in
    for i = 0 to 10 do 
      returner.(i) <- normalized_data.(i)
    done;
  returner
;;
let of_array arr = 
  Array.to_list (Array.map (fun f -> (Array.make 1 f)) arr)
 ;;
let inputs_i = ArrayUtils.transpose (inputs_func (normalized_data))
;;
let inputs = Array.to_list inputs_i
let rec print_list lst = 
  match lst with
  | [] -> ()
  | h::t -> print_string h; print_string "\n"; print_list t
;;

let split = 80 * (List.length inputs) / 100
let testing_labels = List.rev (split_at_point (List.rev labels) split)
let training_labels = split_at_point labels ((List.length inputs) - split)

let testing_inputs = List.rev (split_at_point(List.rev inputs) split)
let training_inputs = split_at_point inputs ((List.length inputs) - split)


let () = 
  print_string "The number of elements in the dataset is:";
  print_int(Array.length data_post); print_string "\n";
  print_string "The length of each element is:";
  print_int(Array.length data_post.(0)); print_string "\n";
  let first_time = Unix.gettimeofday () in
  let network = [11;20;1] in
  let weights = weights_gen network in
  let biases = bias_gen network in

  let trained = train 0.1 training_inputs weights biases training_labels in
  let post_time = (Unix.gettimeofday () -. first_time) in
  print_string "THe total time taken to train the network is: "; print_float (post_time); print_string "\n"; 

  print_int(List.length trained)

  (*Finally, test that the NN learns by running train*)
 
;; 