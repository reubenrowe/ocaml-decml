open Csv
open Utils
let rand_flt = 
  Random.self_init();
  Random.float 0.5
;;

let rec zero_list length =
  match length with
  | 0 -> []
  | h -> (0.)::zero_list (h - 1)
;;

let rec rand_list length = 
  match length with 
  | 0 -> []
  | h -> Random.self_init();
    ((Random.float 1.) -. 0.5)::rand_list (h-1)
;;

let rec lister list = 
  match list with
  | [] -> []
  | h::t -> [h]::lister t
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
let rec print_list_i lst = 
  match lst with
  | [] -> ()
  | h::[] -> print_float h; print_string "]"; print_string "\n";
  | h::t ->   print_float h; print_string "\n"; print_list_i t
;;
let print_list lst =
  print_string "[";
  print_list_i lst;
;;
let print_all lst = 
  List.map print_list lst
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
let string_float_1 list = 
  List.map string_of_float list
;;
let string_float_2 data = 
  List.map string_float_1 data
;;
let floater_1 list = 
  List.map float_of_string list
;;
let floater_2 list = 
  List.map floater_1 list
;;
let least list =  
  List.fold_left (fun a b -> if a < b then a else b)
  (List.hd list) list
;;
let most list =
  List.fold_left (fun a b -> if a > b then a else b)
  (List.hd list) list
;;
let normalize ~element:elem ~minimum:minA ~maximum:maxA = 
  let result = 
  ((elem -. minA)  /. (maxA -. minA)) in
  result
;;
let data_normalizor min max list = 
  List.map (fun x -> normalize x min max) list
;;







(* pipeline to normalize a single list*)
let pipeline list = 
    let least_elem = least list in
      let most_elem = most list in 
        let data_normalized = data_normalizor least_elem most_elem list in
        data_normalized
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
  let network_output = List.hd (List.rev ff_results) in
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
  (*let () = 
    print_string "Testing the errors........\n";
    print_list (List.hd (elem_wise_mat_mult product_t output_der))
  in*)
  List.hd (elem_wise_mat_mult product_t output_der)
;;
let rec error_acc_rec (ff_results:float list list) (weights:float list list list) (error:float list) =
  match ff_results, weights with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths (In error_acc)"
  | [], h::t -> []
  | h::t, h1::t1 -> 
  let error_res = error_acc_func h h1 error in
    (error_res)::error_acc_rec t t1 error_res
;;
let error_acc_1 (ff_results:float list list) (weights:float list list list) (error:float list) = 
 let ff_results_1 = List.tl (List.rev (List.tl ff_results)) in 
  error_acc_rec ff_results_1 weights error
;;
let error_acc (ff_results:float list list) (weights:float list list list) (label:float list) = 
  let reversed_ff =  ff_results in
  let reversed_weights =  List.rev weights in
  let first_err = List.hd (error_acc_init reversed_ff reversed_weights label) in
  (*let () = 
    print_string "The first error is...:";
    print_list (first_err);
    print_string "\n"
  in*)
  let err_list = error_acc_1 reversed_ff reversed_weights first_err in 

  (*let () = 
    print_string "The error vector in the middle is...:";
    print_list (List.hd err_list);
    print_string "\n";
    print_string "---------------------\n"
  in*)
  first_err::(err_list)
;;

let weight_delta (learning_rate:float) (error: float list) (output:float list) = 
  let stretched_output = [output] in
  let stretched_error = [error] in
  let output_t = transpose stretched_output in 
  let returner_a = matrix_product output_t stretched_error in
  let returner = transpose returner_a in
  let returner_1 = (mat_gen (List.length (List.hd returner)) (List.length returner) learning_rate) in
  let product = elem_wise_mat_mult returner returner_1 in
  (*let () = 
    print_string "Weight updater being tested....";
    print_list (List.hd product);
    print_string "\n"
  in*)
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
   (List.rev errors)
;;
let weights_updater (learning_rate:float) (errors:float list list) (outputs:float list list) =
  let outputs_t = (List.tl (List.rev outputs)) in
  let updated = weight_rec learning_rate errors outputs_t in
  List.rev updated
;;

let update_weights(initial_w:float list list list) (deltas:float list list list) =
  List.map2 mat_diff initial_w deltas
;;
(*Returns a list, containing the trained weights, and one element that is a list containing the trained bias*)
let rec train learning_rate data weights bias labels  = 
  match data, labels with
  | [], [] -> weights::[bias]::[]
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
                  let ff_results = output_acc h weights bias in
                  let errors = error_acc ff_results weights h1 in
                  let weight_deltas = weights_updater learning_rate errors ff_results in 
                  let new_weights = update_weights weights weight_deltas in
                  let new_biases = bias_updater errors in

                  train learning_rate t new_weights new_biases t1  
;;
let rec test data weights bias labels =
  match data, labels with
  | [], [] -> 0
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
                  let output = ff_network h weights bias in
                  let () = 
                    print_string "Expected output:"; 
                    print_list h1;
                    print_string "\n"
                  in
                  let () = 
                    print_string "Actual output:";
                    print_list output;
                    print_string "\n"
                  in
                  if ((mse h1 output) > 0.1) then 0 + test t weights bias t1
                  else 1 + test t weights bias t1
;;
          


(*Methods to get the weights and biases from the trianed structure*)
let decoupled_w trained = 
  List.hd trained
;;
let decoupled_b trained =
  List.hd(List.hd(List.rev trained))
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





let rec all_float list = 
  match list with
  | [] -> []
  | h::t -> (float_of_string h)::all_float t
;;
let data_with_headers = Csv.load "wine_dataset.csv"
;;
let data = transp (List.tl data_with_headers)
;;
let labels_string = List.hd(List.rev data)
;;
let inputs_t = List.rev (List.tl (List.rev data))
;;
let inputs_string = transp inputs_t 
;;

let rec wine_encode elem =
  match elem with
  | x when x <= 5. ->  [0.;1.]
  | _ -> [1.;0.]
;;

let float_labels = all_float labels_string
let new_labels = List.map (wine_encode) float_labels
(*let normalized_labels = pipeline float_labels*)
let inputs_i = List.map all_float inputs_string
let inputs = List.map pipeline inputs_i 


(*let labels = lister normalized_labels*)
let labels = new_labels
let data_length = List.length (inputs)
let split = 80 * (data_length) / 100
;;
let testing_labels = List.rev (split_at_point (List.rev labels) split)
let training_labels = split_at_point labels (data_length - split)

let testing_inputs = List.rev (split_at_point(List.rev inputs) split)
let training_inputs = split_at_point inputs (data_length - split)


(*TESTING CODE*)
let () = 
  let network = [11;4;2] in
  let weights = weights_gen network in
  print_string "PRITNING SOME OF THE WEIGHTS BEFORE TESTING";
  let () = 
    print_list(List.hd(List.hd (List.tl weights)))
  in
  let biases = bias_gen network in

  (*let data = [0.3; 0.1; 0.9] in
  let label = [0.1] in

  let ff_results = output_acc data weights biases in
  let errors = error_acc ff_results weights label in

  let new_weights = weights_updater errors ff_results in
  print_int(List.length(List.hd(weights)));
  print_string "\n";
  print_int(List.length (List.hd  new_weights));*)
  


  let rec trainer weights biases index = 
    match index with 
    | 0 -> weights::[biases]::[]
    | h -> let train_i = train 1. training_inputs weights biases training_labels in
      let new_w = decoupled_w train_i in
      let new_b = decoupled_b train_i in
    trainer new_w new_b (index - 1)
  in

  let trained = trainer weights biases 1000 in
  print_int(List.length trained);
  let trained_weights = decoupled_w trained in
  let trained_bias = decoupled_b trained in
  let correct_count = test testing_inputs trained_weights trained_bias testing_labels in
  let accuracy = (float_of_int correct_count) /. (float_of_int (List.length testing_inputs)) in
  print_string "The accuracy of the network was...."; print_float accuracy; print_string "\n";
  print_list (List.hd (List.hd trained_weights))
;;