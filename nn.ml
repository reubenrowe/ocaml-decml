(*The following code is an absolute hairball. Read at your own peril*)


(*Generate a random float*)
let rand_flt = 
  Random.self_init();
  Random.float 1.0
;;
(*Generate an Ocaml array with random elements between 0 and 1*)
let rand_flt_array (length: int) = 
  Array.make length rand_flt
;; 
(*Generate a 2 dimensional Ocaml matrix with random elements between 0 and 1*)
let rand_flt_matrix (length:int) (width:int) = 
  Array.make width (rand_flt_array length)
;;
(*Compute the dot product between two Ocaml arrays*)
let dot_product arr1 arr2 = 
  if Array.length arr1 <> Array.length arr2
  then invalid_arg "Different array lengths";
  let times arr1 arr2 = 
    Array.mapi (fun i arr1_i -> arr1_i  *. arr2.(i)) arr1 in
      Array.fold_left (+.) 0. (times arr1 arr2)
;;
(*Compute the transpose of an Ocaml list*)
let rec transp m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transp (List.map List.tl m)  
(*Use of the above to compute the transpose of an ocaml matrix*)
let transpose matrix = 
    let list_arr = Array.to_list matrix in
    let lst = List.map Array.to_list list_arr in
    let transposed_list = transp lst in
    let arr_list = List.map Array.of_list transposed_list in
    let returner = Array.of_list arr_list in
    returner
;;
(*Compute the matrix product of two Ocaml matrices*)
let matrix_multiply x y =
  let x0 = Array.length x
  and y0 = Array.length y in
  let y1 = if y0 = 0 then 0 else Array.length y.(0) in
  let z = Array.make_matrix x0 y1 0. in
  for i = 0 to x0-1 do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
        z.(i).(j) <- z.(i).(j) +. x.(i).(k) *. y.(k).(j)
      done
    done
  done;
  z
;;
(*Compute the element wise matrix product of two Ocaml matrices*)
let elem_wise_mat_mult x y =
  let x0 = Array.length x
  and y0 = Array.length y in
  if x0 <> y0 then invalid_arg "Different array lengths!";
  let x1 = Array.length x.(0)
  and y1 = Array.length y.(0) in
  if x1 <> y1 then invalid_arg "Different array widths!";
  let z = Array.make_matrix x0 y1 0. in
    for i = 0 to x0-1 do
      for j = 0 to y1-1 do 
        z.(i).(j) <- x.(i).(j) *. y.(i).(j)
      done
    done;
  z
;;
(*Collection of functions to compute the MSE of two arrays*)
let arr_sum (arr1:float array) (arr2:float array) =
  Array.map2 (-.) arr1 arr2
;;

let arr_dif (arr1:float array) (arr2:float array) =
  Array.map2 (-.) arr1 arr2
;;
let arr_square (arr:float array) =
  let square x = x *. x in
    Array.map square arr
;;
let mse (arr1:float array) (arr2:float array) =
  let diff = arr_dif arr1 arr2 in
    let squared = arr_square diff in 
      Array.fold_left (+.) 0. squared
;;
(*Sum element wise of two Ocaml matrices*)
let mat_sum (mat1:float array array) (mat2:float array array) =
  Array.map2 arr_sum mat1 mat2
;;
(*Generate the weights from an int list*)
let rec weights_gen (list:int list) =
    match list with
    [] -> []
    |_::[] -> []
    |first::second::lst ->
      let weight_mat = rand_flt_matrix first second in
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
  let alpha' = dot_product data weights +. bias in
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
  let product_t = matrix_multiply stretched_error weights in 
  let input = Array.map (fun f -> 1. -. f) ff_result in 
  let stretched_input = Array.make 1 input in 
  let stretched_ff_result = Array.make 1 ff_result in
  let output_der = elem_wise_mat_mult stretched_ff_result stretched_input in
  (elem_wise_mat_mult product_t output_der).(0)
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
  let ff_results_t1 = List.tl (List.rev ff_results_t) in
  error_accumulator_2 ff_results_t1 weights error
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
  let output_t = transpose stretched_output in 
    let returner = matrix_multiply output_t stretched_error in
    let returner_1 = (Array.make_matrix (Array.length returner) (Array.length returner.(0)) learning_rate) in
  transpose (elem_wise_mat_mult returner returner_1)
;;
let rec weight_updater_1 (learning_rate: float) (errors: float array list) (outputs: float array list) = 
  match errors, outputs with
  |[], [] -> []
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
  let first = weight_updater_2 learning_rate errors outputs in 
  List.rev first
;;
(*Execute the above in order to update the weights*)
let update_weights (initial_w:float array array list) (deltas:float array array list) =
  List.map2 mat_sum initial_w deltas
;;
(*Find a way to test all of it...*)
let rec train learning_rate data weights bias labels  = 
  match data, labels with
  | [], [] -> ()
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
                  let ff_results = output_accumulator_2 h weights bias in
                  let output = feed_forward_network h weights bias in
                  let errors = error_accumulator_3 ff_results weights h1 in 
                  let weight_deltas = weight_updater_3 learning_rate errors ff_results in 
                  let new_weights = update_weights weights weight_deltas in
                  print_string "The output at this iteration is:   ";
                  print_float (output.(0));
                  print_string "\n";
                  print_string "The MSE of this iteration is:  ";
                  print_float(mse output h1);
                  print_string "\n";
                  train learning_rate t new_weights bias t1  
;;





  
(*TESTING CODE....*)
let head_dimension list = 
  match list with
    [] -> 0
    | h::t -> Array.length h
;;
let () = 
  (*Define the dimension of your network*)
  let network_1 = [3;4;1] in
  let weights = weights_gen network_1 in
  let biases = bias_gen network_1 in 

  (*Then, generate static data to test learning behaviour. The output should eventually match the label*)
  (*Below, the input layer takes an array that contains three lots of 0.5, and overfits on an approximation of pi*)
  let label = Array.make 1 0.314159 in 
  let data = Array.make 3 0.5 in
  let labels_1 = Array.make 500 label in
  let labels = Array.to_list labels_1 in
  let datas_1 = Array.make 500 data in
  let datas = Array.to_list datas_1 in
 

  (*Finally, test that the NN learns by running train*)
  train 0.1 datas weights biases labels 
;; 
