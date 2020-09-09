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
  
