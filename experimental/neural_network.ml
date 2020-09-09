open Csv
open ListUtils

(*Generates the weights from a given list*)
let rec weights_gen (list:int list) = 
  match list with
  | [] -> []
  | h::[] -> []
  | a::b::list -> 
    let weight_mat = ListUtils.rand_mat a b in
    weight_mat::weights_gen(b::list)
;;

(*Generates the bias from a given list*)
let rec bias_gen (list:int list) =
  match list with
  | [] -> []
  | h::[] -> []
  | a::b::list ->
    let bias_list = ListUtils.zero_list b in
    bias_list::bias_gen(b::list)
;;
(*Feed forward operation for a single neuron in a layer*)
let ff_neuron data weights bias =
  let alpha = ListUtils.dot_product data weights +. bias in
  sigmoid alpha
;;
(*Feed forward operation for an entire layer*)
let ff_layer data weights bias = 
  List.map2 (ff_neuron data) weights bias 
;;
(*Feed foreward operation for an entire network*)
let ff_network data weights bias = 
  List.fold_left2 ff_layer data weights bias
;;

(*Function to compute and store all the outputs during the feed forward process*)
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

(*Compute the initial error of the layer layer, and append that to an array*)
let error_acc_init (ff_results:float list list) (weights:float list list list) (label: float list) =
  let network_output = List.hd (List.rev ff_results) in
    let first_error = List.map2 (-.) network_output label in
  first_error::[]
;;
(*Define the function to compute the error for a single layer*)
let error_acc_func (ff_result:float list) (weight:float list list) (error:float list) = 
  let stretched_error = [error] in 
  let product_t = ListUtils.matrix_product stretched_error weight in
  let input = List.map (fun f -> 1. -. f) ff_result in
  let stretched_input = [input] in
  let stretched_ff_result = [ff_result] in
  let output_der = ListUtils.elem_wise_mat_mult stretched_ff_result stretched_input in
  (*let () = 
    print_string "Testing the errors........\n";
    print_list (List.hd (elem_wise_mat_mult product_t output_der))
  in*)
  List.hd (ListUtils.elem_wise_mat_mult product_t output_der)
;;
(*Use the above function to compute all the errors*)
let rec error_acc_rec (ff_results:float list list) (weights:float list list list) (error:float list) =
  match ff_results, weights with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths (In error_acc)"
  | [], h::t -> []
  | h::t, h1::t1 -> 
  let error_res = error_acc_func h h1 error in
    (error_res)::error_acc_rec t t1 error_res
;;
(*Use that function with different data so that it works backwards*)
let error_acc_1 (ff_results:float list list) (weights:float list list list) (error:float list) = 
 let ff_results_1 = List.tl (List.rev (List.tl ff_results)) in 
  error_acc_rec ff_results_1 weights error
;;
(*Do the same once more, and bring it all together to compute the error vectors*)
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

(*Operation for computing a single delta to compute partial derivatives in order to update the weights*)
let weight_delta (learning_rate:float) (error: float list) (output:float list) = 
  let stretched_output = [output] in
  let stretched_error = [error] in
  let output_t = ListUtils.transpose stretched_output in 
  let returner_a = ListUtils.matrix_product output_t stretched_error in
  let returner = ListUtils.transpose returner_a in
  let returner_1 = (ListUtils.mat_gen (List.length (List.hd returner)) (List.length returner) learning_rate) in
  let product = ListUtils.elem_wise_mat_mult returner returner_1 in
  (*let () = 
    print_string "Weight updater being tested....";
    print_list (List.hd product);
    print_string "\n"
  in*)
   product
;;
(*Recursive operation for computing deltas*)
let rec weight_rec (learning_rate:float) (errors:float list list) (outputs:float list list) =
  match errors, outputs with
  | [], [] -> []
  | h::t, [] -> invalid_arg "Different list lengths in weight_rec";
  | [], h::t -> invalid_arg "Different list lengths in weight_rec";
  | h::t, h1::t1 -> 
     let delta_i = weight_delta learning_rate h h1 in
    (delta_i)::weight_rec learning_rate t t1
;;
(*Errors is backwards, so the bias is updated as such*)
let bias_updater (errors:float list list) =
   (List.rev errors)
;;
(*Bring the above together to compute and store all the deltas*)
let weights_updater (learning_rate:float) (errors:float list list) (outputs:float list list) =
  let outputs_t = (List.tl (List.rev outputs)) in
  let updated = weight_rec learning_rate errors outputs_t in
  List.rev updated
;;
(*Use this function to now update the weights, using the computed deltas*)
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
(*A similar method for observing the results of training, on a different test set*)
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
                  if ((ListUtils.mse h1 output) > 0.1) then 0 + test t weights bias t1
                  else 1 + test t weights bias t1
;;
          


(*Methods to get the weights and biases from the trianed structure*)
let decoupled_w trained = 
  List.hd trained
;;
let decoupled_b trained =
  List.hd(List.hd(List.rev trained))
;;

(*Method to split the data up*)
let rec split_at_point l n =
  if n = 0 then
      l
  else
      match l with
      | [] -> []
      | head :: tail ->
          split_at_point tail (n - 1)
;;




(*Methods to extract the information from a csv file*)
let rec all_float list = 
  match list with
  | [] -> []
  | h::t -> (float_of_string h)::all_float t
;;
let data_with_headers = Csv.load "wine_dataset.csv"
;;
let data = ListUtils.transp (List.tl data_with_headers)
;;
let labels_string = List.hd(List.rev data)
;;
let inputs_t = List.rev (List.tl (List.rev data))
;;
let inputs_string = ListUtils.transp inputs_t 
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
  let network = [11;20;2] in
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
    | h -> let train_i = train 0.01 training_inputs weights biases training_labels in
      let new_w = decoupled_w train_i in
      let new_b = decoupled_b train_i in
    trainer new_w new_b (index - 1)
  in
    let first_time = Unix.gettimeofday () in

  let trained = trainer weights biases 1 in
  print_int(List.length trained);
  let trained_weights = decoupled_w trained in
  let trained_bias = decoupled_b trained in
  let correct_count = test testing_inputs trained_weights trained_bias testing_labels in
  let accuracy = (float_of_int correct_count) /. (float_of_int (List.length testing_inputs)) in
  print_string "The accuracy of the network was...."; print_float accuracy; print_string "\n";
  let last_time = (Unix.gettimeofday () -. first_time) in
  print_string "Total time taken for execution: "; print_float last_time; print_string "\n";
  print_list (List.hd (List.hd trained_weights))
;;