let rand_flt = 
  Random.self_init();
  Random.float 1.0
;;

let rec zero_list length =
  match length with
  | 0 -> []
  | h -> (0.)::zero_list (h - 1)
;;

let rec rand_list length = 
  match length with 
  | 0 -> []
  | h -> (rand_flt)::rand_list (h -1)
;;

let rec rand_mat length width =
  match width with
  | 0 -> []
  | h -> let appender = rand_list length in
  (appender)::rand_mat length (width - 1)
;;
let rec list_gen length elem =
  match length with 
  | 0 -> []
  | h -> elem::list_gen (length - 1) elem
;;
let rec mat_gen length width elem =
  match width with 
  | 0 -> []
  | h -> let appender = list_gen length elem in 
    appender:: mat_gen length (width - 1) elem
;;
let rec dot_product list1 list2 = 
  assert (List.length list1 == List.length list2);
  match list1, list2 with
  | [], [] -> 0.
  | _::_, [] | ([], _::_) -> invalid_arg "Different list lengths in dot_product";
  | h::t, h1::t1 -> (h *. h1) +. dot_product t t1
;;
let rec transp m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transp (List.map List.tl m)  
;;
let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows)
;;
let rec matrix_product matA matB = 
    let cols = transp matB in 
    List.map (fun row -> List.map (dot_product row) cols) matA
;;
let elem_wise_mat_mult matA matB = 
  let list_mult elem1 elem2 = 
    List.map2 (fun f1 f2 -> f1 *. f2) elem1 elem2 in
  List.map2 list_mult matA matB
;;  
let mat_diff matA matB = 
    let list_diff elem1 elem2 = 
      List.map2 (-.) elem1 elem2 in
    List.map2 list_diff matA matB
;;
let mat_sum matA matB = 
    let list_sum elem1 elem2 = 
      List.map2 (+.) elem1 elem2 in 
    List.map2 list_sum matA matB
;;
let sigmoid x = 
    1. /. (1. +. (exp (-1. *. x) ))
;;
let mse list1 list2 = 
  let list_diff lst1 lst2 = 
    List.map2 (-.) lst1 lst2 in
  let first_diff = list_diff list1 list2 in
  let list_square lst = 
    List.map (fun f -> f *. f) lst in
  let second_diff = list_square first_diff in
  List.fold_left (+.) 0. second_diff
;;



let rec weights_gen (list:int list) = 
  match list with
  | [] -> []
  | h::[] -> []
  | a::b::list -> 
    let weight_mat = rand_mat a b in
    weight_mat::weights_gen(b::list)
;;


let rec bias_gen (list:int list) =
  match list with
  | [] -> []
  | h::[] -> []
  | a::b::list ->
    let bias_list = zero_list b in
    bias_list::bias_gen(b::list)
;;
let ff_neuron data weights bias =
  let alpha = dot_product data weights +. bias in
  sigmoid alpha
;;
let ff_layer data weights bias = 
  List.map2 (ff_neuron data) weights bias 
;;
let ff_network data weights bias = 
  List.fold_left2 ff_layer data weights bias
;;

let output_acc data weights bias = 
  let rec output_acc_rec data weights bias = 
    let weights_head = List.hd weights in
    let bias_head = List.hd bias in
  
      if List.length weights <> List.length bias then
      invalid_arg "Different list lengths";
    match weights, bias with
    | [], [] -> []
    | h::t, [] -> invalid_arg "Different list lengths"
    | [], h::t -> invalid_arg "Different list lengths"
    | h::[], h1::[] -> (ff_layer data h h1)::[]
    | h::t, h1::t1 -> 
    let data_1 = ff_layer data weights_head bias_head in
     (data_1)::output_acc_rec data_1 t t1 in
  data::(output_acc_rec data weights bias)
;;

let error_acc_init (ff_results:float list list) (weights:float list list list) (label: float list) =
  let network_output = List.hd ff_results in
    let first_error = List.map2 (-.) network_output label in
  first_error::[]
;;
let error_acc_func (ff_result:float list) (weight:float list list) (error:float list) = 
  let stretched_error = [error] in
  let product_t = matrix_product stretched_error weight in
  let input = List.map (fun f -> 1. -. f) ff_result in
  let stretched_input = [input] in
  let stretched_ff_result = [ff_result] in
  let output_der = elem_wise_mat_mult stretched_ff_result stretched_input in
  List.hd (elem_wise_mat_mult product_t output_der)
;;
let rec error_acc_rec (ff_results:float list list) (weights:float list list list) (error:float list) =
  match ff_results, weights with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths (In error_acc)"
  | [], h::t -> []
  | h::t, h1::t1 -> let error_res = error_acc_func h h1 error in
    (error_res)::error_acc_rec t t1 error_res
;;
let error_acc_1 (ff_results:float list list) (weights:float list list list) (error:float list) = 
 let ff_results_1 = List.tl ff_results in 
  error_acc_rec ff_results_1 weights error
;;
let error_acc (ff_results:float list list) (weights:float list list list) (label:float list) = 
  let reversed_ff = List.rev ff_results in
  let reversed_weights = List.rev weights in
  let first_err = List.hd (error_acc_init reversed_ff reversed_weights label) in
  let err_list = error_acc_1 reversed_ff reversed_weights first_err in 
  first_err::(err_list)
;;

let weight_delta (learning_rate: float) (error: float list) (output:float list) = 
  let stretched_output = [output] in
  let stretched_error = [error] in
  let output_t = transpose stretched_output in 
  let returner_a = matrix_product output_t stretched_error in
  let returner = transpose returner_a in
  let returner_1 = (mat_gen (List.length (List.hd returner)) (List.length returner) learning_rate) in
  let product = elem_wise_mat_mult returner returner_1 in
   product
;;
let rec weight_rec (learning_rate:float) (errors:float list list) (outputs:float list list) =
  match errors, outputs with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths in weight_rec";
  | [], h::t -> invalid_arg "Different list lengths in weight_rec";
  | h::t, h1::t1 -> 
     let delta_i = weight_delta learning_rate h h1 in
    (delta_i)::weight_rec learning_rate t t1
;;

let bias_updater (errors:float list list) =
  List.tl (List.rev errors)
;;
let weights_updater (learning_rate:float) (errors:float list list) (outputs:float list list) =
  let errors_t = List.rev (List.tl (List.rev errors)) in
  let outputs_t = List.tl (List.rev outputs) in
  let updated = weight_rec learning_rate errors_t outputs_t in
  List.rev updated
;;

let update_weights (initial_w:float list list list) (deltas:float list list list) =
  List.map2 mat_diff initial_w deltas
;;

let rec train learning_rate data weights bias labels  = 
  match data, labels with
  | [], [] -> ()
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
                  let ff_results = output_acc h weights bias in
                  let output = ff_network h weights bias in
                  let errors = error_acc ff_results weights h1 in
                  let weight_deltas = weights_updater learning_rate errors ff_results in 
                  let new_weights = update_weights weights weight_deltas in
                  let new_biases = bias_updater errors in
                  print_string "The output at this iteration is:   ";
                  print_float (List.hd output);
                  print_string "\n";
                  print_string "The MSE of this iteration is:  ";
                  print_float(mse h1 output);
                  print_string "\n";
                  train learning_rate t new_weights new_biases t1  
;;
(*TESTING CODE*)
let () = 
  let network_1 = [3;4;1] in
  let weights = weights_gen network_1 in
  let biases = bias_gen network_1 in 

  let label = list_gen 1 0.55 in
  let data = list_gen 3 rand_flt in
  let labels = list_gen 50 label in
  let datas = list_gen 50 data in

  train 0.1 datas weights biases labels 
;;