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
(*Compute the transpose of an Ocaml array*)
let transpose arr = 
  let arr' = Array.make_matrix (Array.length arr.(0)) (Array.length arr) 0. in
    for i = 0 to Array.length arr do 
      let row_i = arr.(i) in
        for j = 0 to Array.length row_i do 
          arr'.(j).(i) <- row_i.(j)
        done
      done;
    arr'
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
(*Feed forward over the entire network*)















let error_accumulator_0 (ff_results:float array list)
 (weights:float array array list)
  (label:float array) =
    let network_output = List.hd (ff_results) in
     let first_error = Array.map2 (-.) network_output label in
  first_error::[]
;;List.tl
let error_accumulator_1 (ff_result:float array)
(weights:float array array)
(error:float array ) = 
  let stretched_error = Array.make 1 error in
  let product_t = matrix_multiply stretched_error weights in 
  print_string "Product_t has dimension ";
  print_int (Array.length product_t.(0));
  print_string "\n";
  let input = Array.map (fun f -> 1. -. f) ff_result in 
  let stretched_input = Array.make 1 input in 
  let stretched_ff_result = Array.make 1 ff_result in
  let output_der = elem_wise_mat_mult stretched_ff_result stretched_input in
  print_string "Output_der has dimension ";
  print_int (Array.length output_der.(0));
  print_string "\n";
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
    print_string("Pending successful appending...");
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
  print_int(List.length err_list);
  first_err::(err_list)
;;
















  
(*TESTING CODE....*)
let head_dimension list = 
  match list with
    [] -> 0
    | h::t -> Array.length h
;;
let () = 
  let network_1 = [3;4;1] in
  let label = Array.make 1 0.7 in 
  let data = Array.make 3 0.5 in
  let weights = weights_gen network_1 in
  let biases = bias_gen network_1 in 
  let network_result = feed_forward_network data weights biases in


  let ff_results = output_accumulator data weights biases in
  let error_result = Array.map2 (-.) network_result label in 
  let err_results = error_accumulator_3 (ff_results) (weights) error_result in
  print_string "\n";
  print_int(Array.length (List.hd (List.rev err_results)))
;;