open Matrix
open Csv

type m = float list list
type t = float list
type n = int list

let rec dot_product list1 list2 = 
  assert (List.length list1 == List.length list2);
  match list1, list2 with
  | [], [] -> 0.
  | _::_, [] | ([], _::_) -> invalid_arg "Different list lengths in dot_product";
  | h::t, h1::t1 -> (h *. h1) +. dot_product t t1

let rec rand_structure length = 
  match length with 
    | 0 -> []
    | h -> Random.self_init();
      ((Random.float 1.) -. 0.5)::rand_structure (h-1)

let rec rand_mat length width =
  match width with
    | 0 -> []
    | h -> let appender = rand_structure length in
      (appender)::rand_mat length (width - 1)

let length_of list = 
  List.length list

let difference list1 list2 = 
  List.map2 (-.) list1 list2

let struct_min list = 
  List.fold_left (fun a b -> if a < b then a else b)
  (List.hd list) list

let struct_max list =
  List.fold_left (fun a b -> if a > b then a else b)
  (List.hd list) list

let normalize_struct list = 
  let min = struct_min list in
  let max = struct_max list in 
  List.map (fun f -> ((f -. min) /. (max -. min))) list

let array_to_vec arr = 
  Array.to_list arr 

let array_to_mat arr = 
   Array.to_list(Array.map array_to_vec arr)
  
let make input = [input]

let mse label output = 
  let list_diff lst1 lst2 = 
    List.map2 (-.) lst1 lst2 in
  let first_diff = list_diff label output in
  let list_square lst = 
    List.map (fun f -> f *. f) lst in
  let second_diff = list_square first_diff in
  List.fold_left (+.) 0. second_diff

let hadamard matA matB = 
  let list_mult elem1 elem2 = 
    List.map2 (fun f1 f2 -> f1 *. f2) elem1 elem2 in
  List.map2 list_mult matA matB

let multiply_map (data:t) (weight_mat: m) (bias:t) =
  let sigmoid elem =
    1. /. (1. +. (exp (-.elem))) in
  let ff_neuron (data:t) (weight_elem: t) (bias:float)  =
    let result = dot_product data weight_elem in
      sigmoid (result +. bias) in
  List.map2 (ff_neuron data) weight_mat bias
let lr_mat (learning_rate:float) (input:float list list) =

  List.map (List.map (fun f -> f *. learning_rate)) input

let rec transpose m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transpose (List.map List.tl m)  

let matrix_multiply (matA:m) (matB:m) =
  let cols = transpose matB in 
  List.map (fun row -> List.map (dot_product row) cols) matA

let print_head mat = 
  let head = List.hd mat in
  List.iter (fun f -> print_string"Testing the head: "; print_float f; print_string "\n") head

let length mat = 
  List.length mat 

let mat_diff matA matB = 
  let list_diff elem1 elem2 = 
    List.map2 (-.) elem1 elem2 in
  List.map2 list_diff matA matB

let get_head (t : 'a list) = List.hd t

let add list1 list2 = List.map2 (+.) list1 list2

let test_errors (errors:t list) =
  List.iter (fun f -> print_string "TESTING THE DIMENSIONS OF THE STRUCTURE : "; print_int (List.length f); print_string "\n") errors

let data file_name = 
  let rec all_float list = 
    match list with
    | [] -> []
    | h::t -> (float_of_string h)::all_float t
      in
  let holder = transpose (List.tl (load file_name)) in
  let data_post = List.map all_float holder in

  let normalized_data = List.map normalize_struct data_post in 
  transpose (List.rev(List.tl(List.rev normalized_data)))

let labels file_name = 
  let rec all_float list = 
    match list with
    | [] -> []
    | h::t -> (float_of_string h)::all_float t
      in
    let holder = transpose (List.tl (load file_name)) in
    let data_post = List.map all_float holder in

  let normalized_data = List.map normalize_struct data_post in
  (transpose [(List.hd(List.rev normalized_data))])

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


let rec weights_gen (list:int list) = 
  match list with
  | [] -> []
  | h::[] -> []
  | a::b::list -> 
    let weight_mat = rand_mat a b in
    weight_mat::weights_gen(b::list) 

let test_weight_dim mat = 
  print_string "This matrix is : "; print_int (List.length mat) ; print_string(" by "); print_int(List.length(List.hd mat)); print_string "\n"

let rec bias_gen (list:int list) =
  let rec zero_list length =
    match length with
    | 0 -> []
    | h -> (0.)::zero_list (h - 1)
  in
  match list with
  | [] -> []
  | h::[] -> []
  | a::b::list ->
    let bias_list = zero_list b in
    bias_list::bias_gen(b::list)

let feed_forward (data:t) (weights:m list) (biases: t list) = 
  List.fold_left2 multiply_map data weights biases
  
let output_accumulator data weights bias = 
  let rec output_acc_rec data weights bias = 
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
      (data_1)::output_acc_rec data_1 t t1 in
  data::(output_acc_rec data weights bias)

let compute_errors (data:t) (label:t) (weights: m list) (biases: t list) =
  (*TODO: LEAD DATA IS NOT BEING APPENDED TO THE ERRORS. USE THIS REFERENCE TO FIX LATER*)
  let outputs = (output_accumulator data weights biases) in
  let error_init = (List.map2 (-.) (List.hd ((List.rev outputs))) label) in

  let error_function (output) (weight) (error) =
    let stretched_error =  [error] in
    let product_t = matrix_multiply stretched_error weight in 
    let input = List.map (fun f -> 1. -. f) output in 
    let stretched_input =  [input] in 
    let stretched_ff_result =  [output] in
    let output_der = hadamard stretched_ff_result stretched_input in
  List.hd((hadamard product_t output_der))
  in

  let rec error_acc (outputs) (weights) (error) = 
    match outputs, weights with
    | [], [] -> []
    | h::t, [] -> invalid_arg "Different list lengths (In error_acc)"
    | [], h::t -> invalid_arg "Different list lengths (In error acc)"
    | h::t, h1::t1 -> let error_res = error_function h h1 error in
      (error_res)::error_acc t t1 error_res
       in
  let returner = error_init::(error_acc (List.tl (List.rev (List.tl outputs))) ((List.rev (List.tl weights))) error_init) in
  returner
  
let update_weights (learning_rate:float) (errors:t list) (outputs: t list) (weights: m list) =
  let weight_delta (learning_rate:float) (error:t) (output:t) =
    let stretched_output =  [output] in
    let stretched_error =  [error] in
    let output_t = transpose stretched_output in 
    let returner = matrix_multiply output_t stretched_error in
      let ret = transpose (lr_mat learning_rate returner) in
      ret in
  let rec delta_compute (learning_rate:float) (errors: t list) (outputs: t list) =
        match errors, outputs with
        | [], [] -> []
        | h::t, [] -> invalid_arg "Different list lengths in weight_updater"
        | [], h::t  -> invalid_arg "Different list lengths in weight_updater"
        | h::t, h1::t1 -> let delta_i =  weight_delta learning_rate h h1 in
        (delta_i)::delta_compute learning_rate t t1
        in
      let new_deltas = List.rev (delta_compute learning_rate (errors)  (List.tl(List.rev outputs))) in
      List.map2 mat_diff new_deltas weights

  let update_biases (learning_rate:float) (errors: t list) (biases: t list) = 
    let use_errs = List.rev errors in
    let product_1 arr1 arr2 = 
      List.map2 (-.) arr1 arr2 
    in
    let learning_mult arr1 = 
      List.map (fun f -> f *. (learning_rate)) arr1 
    in
    let result_1 = List.map2 product_1 use_errs biases
    in
    let result = List.map learning_mult result_1 in
    result
  


