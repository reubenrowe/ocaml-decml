open ListMatrix

let data_split (splitter) (data) = 
  if(splitter > 1.0 || splitter < 0.0) then invalid_arg "Splitter has to be between 0 and 1!";
  let length = List.length data in
  let split_index = int_of_float(splitter *. (float_of_int(length))) in
  let rec split_at_point l n =
    if n = 0 then
        l
    else
        match l with
        | [] -> []
        | head :: tail ->
            split_at_point tail (n - 1)
  in
  let training = split_at_point data (length - split_index) in
  let testing = List.rev (split_at_point (List.rev data) split_index) in
  (training, testing)


let rec train learning_rate data (weights:m list) (bias: t list) labels  = 
  match data, labels with
  | [], [] -> (weights, bias)
  | h::t, [] -> invalid_arg "data and labels have different lengths"
  | [], h::t -> invalid_arg "data and labels have different lengths"
  | h::t, h1::t1 -> 
                  let ff_results =  output_accumulator h weights bias in
                  let errors = compute_errors h h1 weights bias in
                  let new_weights = update_weights learning_rate errors ff_results weights in
                  let new_biases = update_biases learning_rate errors bias in
                  train learning_rate t new_weights new_biases t1  

let rec test data weights biases labels =
  match data, labels with
    | [], [] -> 0
    | h::t, [] -> invalid_arg "data and labels have different lengths"
    | [], h::t -> invalid_arg "data and labels have different lengths"
    | h::t, h1::t1 -> 
      let output = feed_forward h weights biases in 
      if ((mse output h1) > 0.01) then 0 + test t weights biases t1
      else 1 + test t weights biases t1

let () = 
  let network = [11;500;1;1] in
  let weights = weights_gen network in
  let biases = bias_gen network in
  let data = data ("wine_dataset.csv") in
  let labels = labels("wine_dataset.csv") in
  let t = Sys.time() in 
  let (training_data, testing_data) = data_split 0.8 data in
  let (training_labels, testing_labels) = data_split 0.8 labels in
  let (params, biases_1) = train 0.01 training_data weights biases training_labels in
  print_string "The execution speed was: "; print_float (Sys.time() -. t); print_string "\n";
  let accuracy = test testing_data params biases_1 testing_labels in
  print_string "The accuracy of this model was:  ";
  print_float (float_of_int (accuracy) /. float_of_int((List.length testing_data)))