let least list =  
  List.fold_left (fun a b -> if a < b then a else b)
  (List.hd list) list
;;
let most = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs
;;
let normalize ~element:elem ~minimum:minA ~maximum:maxA = 
  let result = 
  ((elem -. minA)  /. (maxA -. minA)) in
  result
;;
let data_normalizor min max list = 
  List.map (fun x -> normalize x min max) list
;;
let pipeline list = 
    let least_elem = least list in
      let most_elem = most list in 
        let data_normalized = data_normalizor least_elem most_elem list in
        data_normalized
;;
let main list = 
  List.map (fun l -> pipeline l) list
;;