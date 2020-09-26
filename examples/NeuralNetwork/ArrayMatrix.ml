open Matrix
open Csv
(*module Array1 : Matrix.S = 
  struct
  type t = float array
  let rand_structure length = 
    let rand_flt () = 
      Random.self_init();
      Random.float 1.0 in
    let returner_1 = Array.make length 0. in
    for i = 0 to (Array.length returner_1) - 1 do
      returner_1.(i) <- rand_flt() 
    done;
    returner_1 
  let length_of arr =
    Array.length arr
    end*)        


(* module ArrayMatrix : Matrix.Matrix = 
  struct  *)
  type m = float array array
  type t = float array
  type n = int list

  let dot_product (arr1:t) (arr2:t) =
    if Array.length arr1 <> Array.length arr2
    then invalid_arg "Different array lengths";
    let times arr1 arr2 = 
      Array.mapi (fun i arr1_i -> arr1_i  *. arr2.(i)) arr1 in
        Array.fold_left (+.) 0. (times arr1 arr2)

  let length_of arr =
    Array.length arr

  let add (arr1:t) (arr2:t) = 
    Array.map2 (+.) arr1 arr2
    
  let rand_structure length = 
    let rand_flt () = 
      Random.self_init();
      (Random.float 1.0) -. 0.5 in
      let returner_1 = Array.make length 0. in
        for i = 0 to (Array.length returner_1) - 1 do
          returner_1.(i) <- rand_flt() 
      done;
    returner_1 

  let difference (arr1) (arr2) =
    Array.map2 (-.) arr1 arr2
  ;;

  let get_head (arr:t) = arr.(0)


  let struct_min (arr:float array) =
    let storer = Array.make 1 0. in
    for i = 0 to (Array.length arr) - 1 do
      if arr.(i) < storer.(0) then storer.(0) <- arr.(i) 
    done;
    storer.(0)
  ;; 


  let struct_max (arr:float array) = 
    let storer = Array.make 1 0. in
    for i = 0 to (Array.length arr) - 1 do 
      if arr.(i) > storer.(0) then storer.(0) <- arr.(i)
    done;
    storer.(0)
  ;;

  let normalize_struct arr = 
    let min = struct_min arr in
    let max = struct_max arr in 
    Array.map (fun f -> ((f -. min) /. (max -. min))) arr

    

  let train_split percentage input = 
    let split = percentage * (List.length input) / 100  in
    let rec split_at_point l n =
      if n = 0 then
          l
      else
          match l with
          | [] -> []
          | head :: tail ->
              split_at_point tail (n - 1) in

  split_at_point input ((List.length input) - split)

  let test_split percentage input = 
      let split = percentage * (List.length input) / 100  in
      let rec split_at_point l n =
        if n = 0 then
            l
        else
            match l with
            | [] -> []
            | head :: tail ->
                split_at_point tail (n - 1) in
  
    List.rev (split_at_point(List.rev input) split)
    
  let float_mat string_mat = 
    let returner = Array.make_matrix (Array.length string_mat) (Array.length string_mat.(0)) 0. in
    for i = 0 to (Array.length string_mat) - 1 do 
      for j = 0 to (Array.length string_mat.(0)) - 1 do
        returner.(i).(j) <- float_of_string (string_mat.(i).(j))
      done
    done;
    returner
  ;;




  let mat_diff (mat1) (mat2) = 
    Array.map2 (Array.map2 (-.)) mat1 mat2
  
  let rand_mat (length:int) (width:int) = 
    let rand_struct length = 
      let rand_flt () = 
        Random.self_init();
        Random.float 1.0 in
      let returner_1 = Array.make length 0. in
      for i = 0 to (Array.length returner_1) - 1 do
        returner_1.(i) <- rand_flt() 
      done;
      returner_1 in

    let returner = (Array.make_matrix length width 0.) in
      for i = 0 to (length)- 1 do 
        let appender = rand_struct width in
          returner.(i) <- appender
      done;
  returner


  let length m = 
    Array.length m

  
  let transpose matrix = 
    let returner = Array.make_matrix (Array.length matrix.(0)) (Array.length matrix) 0. in
    for i = 0 to (Array.length matrix) - 1 do
      for j = 0 to (Array.length matrix.(0)) - 1 do
        returner.(j).(i) <- matrix.(i).(j)
      done
    done;
  returner
  let multiply_map (data:t) (weight_mat:m) (bias:t) = 
    let sigmoid elem =
      1. /. (1. +. (exp (-.elem))) in
      let ff_neuron (data:t) (weight_elem:t) (bias:float) = 
        let result = dot_product data weight_elem in
          sigmoid (result +. bias) in
    Array.map2 (ff_neuron data) weight_mat bias 

  let data (file_name:string) = 
    let data_post = transpose (float_mat(Array.of_list (List.map Array.of_list (List.tl (load file_name))))) in
    let normalized_data = Array.map (normalize_struct) data_post in
    let input_func (mat:float array array) = 
      let returner = Array.make_matrix ((Array.length mat)- 1) (Array.length(mat.(0))) 0. in
      for i = 0 to 10 do 
        returner.(i)  <- normalized_data.(i)
      done;
    returner in
   let final = Array.to_list (transpose (input_func (normalized_data))) in 
   final

;;

  let array_to_mat a = a
  let array_to_vec a = a


  let labels (file_name:string) = 
    let data_post = transpose (float_mat(Array.of_list (List.map Array.of_list (List.tl (load file_name))))) in
    let normalized_data = Array.map (normalize_struct) data_post in
    Array.to_list (Array.map (fun f -> (Array.make 1 f)) (normalized_data.(Array.length normalized_data - 1)))


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

  let hadamard x y =
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
  let make (input:t) = 
    Array.make 1 input
  let mse (input:t) (input2:t) = 
    let difference = Array.map2 (-.) input input2 in
    Array.fold_left (+.) 0. (Array.map (fun f -> f *. f) difference)


  let print_head m = 
    let length = Array.length (m.(0)) in
    for i = 0 to length - 1 do
      print_string "Testing head : "; print_float (m.(0).(i)); print_string "\n"
    done;
    print_string("Complete!")
  let rec weights_gen (list:int list) = 
    match list with
    [] -> []
    |_::[] -> []
    |first::second::lst ->
      let weight_mat = rand_mat second first in
      weight_mat::(weights_gen (second::lst))
  
  let rec bias_gen (list:int list) = 
    match list with
      [] -> []
      | _::[] -> []
      |first::second::lst ->
        let bias_vec = Array.make second 0. in
        bias_vec::(bias_gen(second::lst))

  let feed_forward (data:t) (weights:m list) (biases: t list) = 
    List.fold_left2 multiply_map data weights biases
  let rec output_accumulator_func data (weights:m list) bias = 
    let weights_head = List.hd weights in
    let bias_head = List.hd bias in
    
    if List.length weights <> List.length bias then
    invalid_arg "Different list lengths";
    match weights, bias with
      | [], [] -> []
      | h::t, [] -> invalid_arg "Different list lengths"
      | [], h::t -> invalid_arg "Different list lengths"
      | h::[], h1::[] -> (multiply_map data h h1)::[]
      | h::t, h1::t1 -> 
        let data_1 = multiply_map data weights_head bias_head in
        (data_1)::output_accumulator_func data_1 t t1
  let output_accumulator data weights bias =
    data::(output_accumulator_func data weights bias)
  let compute_errors (data:t) (label:t) (weights: m list) (biases: t list) =
    (*TODO: LEAD DATA IS NOT BEING APPENDED TO THE ERRORS. USE THIS REFERENCE TO FIX LATER*)
    let outputs = data::(output_accumulator data weights biases) in
    let error_init = (Array.map2 (-.) (List.hd ((List.rev outputs))) label) in

    let error_function (output) (weight) (error) =
      let stretched_error = Array.make 1 error in
      let product_t = matrix_multiply stretched_error weight in 
      let input = Array.map (fun f -> 1. -. f) output in 
      let stretched_input = Array.make 1 input in 
      let stretched_ff_result = Array.make 1 output in
      let output_der = hadamard stretched_ff_result stretched_input in
    (hadamard product_t output_der).(0) 
    in



    let rec error_acc (outputs) (weights) (error) = 
    match outputs, weights with
    | [], [] -> []
    | h::t, [] -> invalid_arg "Different list lengths (In error_acc)"
    | [], h::t -> []
    | h::t, h1::t1 -> let error_res = error_function h h1 error in
      (error_res)::error_acc t t1 error_res
       in
  error_init::(error_acc (List.tl(List.rev (List.tl (List.tl outputs)))) (List.rev (List.tl weights)) error_init)

  let rec test_errors (errors) = 
    match errors with
    | [] -> ()
    | h::t -> print_string "TESTING THE DIMENSION OF ERRORS: "; print_int (Array.length h); print_string "\n";
      test_errors t

  let update_weights (learning_rate:float) (errors:t list) (outputs: t list) (weights: m list) =
    let weight_delta (learning_rate:float) (error:t) (output:t) =
      let stretched_output = Array.make 1 output in
      let stretched_error = Array.make 1 error in
      let output_t = transpose stretched_output in 
      let returner = matrix_multiply output_t stretched_error in
      let returner_1 = (Array.make_matrix (Array.length returner) (Array.length returner.(0)) learning_rate) in
         let ret = transpose (hadamard returner returner_1) in
        ret in
    
    let rec delta_compute (learning_rate:float) (errors: t list) (outputs: t list) =
      match errors, outputs with
      | [], [] -> []
      | h::t, [] -> invalid_arg "Different list lengths in weight_updater"
      | [], h::t  -> invalid_arg "Different list lengths in weight_updater"
      | h::t, h1::t1 -> let delta_i =  weight_delta learning_rate h h1 in
      (delta_i)::delta_compute learning_rate t t1
      in
    let new_deltas = List.rev (delta_compute learning_rate ( errors)  (List.tl(List.rev outputs))) in
    List.map2 mat_diff new_deltas weights
    
  let update_biases (learning_rate:float) (errors: t list) (biases: t list) = 
    let use_errs = List.rev errors in
    let product_1 arr1 arr2 = 
      Array.map2 (-.) arr1 arr2 
    in
    let learning_mult arr1 = 
      Array.map (fun f -> f *. (learning_rate)) arr1 
    in
    let result_1 = List.map2 product_1 use_errs biases
    in
    let result = List.map learning_mult result_1 in
    result



  (* end *)

(* open ArrayMatrix
let rec train learning_rate data (weights:m list) (bias: t list) labels  = 
    match data, labels with
    | [], [] -> (weights, Array.of_list bias)
    | h::t, [] -> invalid_arg "data and labels have different lengths"
    | [], h::t -> invalid_arg "data and labels have different lengths"
    | h::t, h1::t1 -> 
                    let ff_results = output_accumulator h weights bias in
                    (*let outputs = feed_forward h weights bias in*)
                    let errors = compute_errors h h1 weights bias in 
                    let new_weights = update_weights learning_rate errors ff_results weights in
                    let new_biases = update_biases errors in
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
  let network = [2;2] in
  let weights = ArrayMatrix.weights_gen network in
  let biases = ArrayMatrix.bias_gen network in
  let data = ArrayMatrix.array_to_vec [|0.3; 0.5|] in
  let label = ArrayMatrix.array_to_vec [|0.5; 0.5|] in
  let alpha = 
      (ArrayMatrix.compute_errors data label weights biases) (* t *)
     (* t *) in 
  print_float(ArrayMatrix.get_head (List.hd (List.tl alpha)))
 *)
