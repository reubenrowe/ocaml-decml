open Csv
let head_drop lst = 
  match lst with
  |[] -> []
  |h::t -> t
;;

let data_1 = Csv.load "bh.csv"
let headers = List.hd data_1
let data_2 = List.tl data_1
let data = Csv.transpose data_2
;;
(*Collection of useful methods to compose and test our normalization*)
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

let rec print_list = function 
  [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l
;;
let print_all lst = 
  List.iter (fun f -> print_list f) lst
;;





(* pipeline to normalize a single list*)
let pipeline list = 
    let least_elem = least list in
      let most_elem = most list in 
        let data_normalized = data_normalizor least_elem most_elem list in
        data_normalized
;;
(*Apply the pipeline function to each list of string in the csv file*)
let result = 
  List.map(fun l -> pipeline l)
;;
(*Integrate code for a file name*)
let pipleline_2 file_name = 
  let data_1 = Csv.load file_name in 
    let headers = List.hd data_1 in
      let data_2 = List.tl data_1 in
        let data = Csv.transpose data_2 in
          let transformed = floater_2 data in 
            let normalized = result transformed in 
              let str_normalized = string_float_2 normalized in 
                let str_transpose = Csv.transpose str_normalized in 
                  let str_result = headers::str_transpose in 
                  str_result
;;