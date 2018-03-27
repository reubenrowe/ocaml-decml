open Csv

type 'a t = ('a * Carrier.t) list

let of_csv_file 
    ?separator ?strip ?backslash_escape ?excel_tricks ?fix ?(header=true) 
    build f =
  let data = load ?separator ?strip ?backslash_escape ?excel_tricks ?fix f in
  let data =
    match data, header with
    | _::data, true ->
      data
    | _ ->
      data in
  build data