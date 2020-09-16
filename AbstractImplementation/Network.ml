open ArrayMatrix
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
      if ((mse output h1) > 0.05) then 0 + test t weights biases t1
      else 1 + test t weights biases t1

let () = 
  let network = [11;20;1] in
  let weights = weights_gen network in
  let biases = bias_gen network in
  let data = data ("wine_dataset.csv") in
  let labels = labels("wine_dataset.csv") in
  let (training_data, testing_data) = data_split 0.8 data in
  let (training_labels, testing_labels) = data_split 0.8 labels in
  let (params, biases) = train 0.01 training_data weights biases training_labels in
  let accuracy = test testing_data params biases testing_labels in
  print_string "The accuracy of this model was:  ";
  print_float (float_of_int (accuracy) /. float_of_int((List.length testing_data)))