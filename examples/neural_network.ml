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
  let arr' = Array.make_matrix (Array.length arr.(0)) (Array.length arr) 0 in
    for i = 0 to Array.length arr do 
      let row_i = arr.(i) in
        for j = 0 to Array.length row_i do 
          arr'.(j).(i) <- row_i.(j)
        done
      done;
    arr'
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
      let bias_vec = Array.make second 0 in
      bias_vec::(bias_gen(second::lst))
;;
(*Compute feed forward for a single neuron*)
let feed_forward_neuron data weights bias =
  let forward = dot_product data weights in
    forward +. bias
;;
(*Compute feed forward for the next layer*)
let feed_forward_layer data layer weights bias =
  Array.map (feed_forward_neuron (data weights bias)) layer
;;
(*TESTING CODE....*)
let head_dimension list = 
  match list with
    [] -> 0
    | h::t -> Array.length h
;;
let () = 
  let list_test = [3;4;1] in
    let tester = make_bias list_test in
    print_int (head_dimension tester)
;;