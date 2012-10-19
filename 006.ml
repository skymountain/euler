let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let sum = List.fold_left (+) 0

let sum_of_squares l = sum (List.map (fun x -> x * x) l)

let square_of_sum l =
  let sum' = sum l in
  sum' * sum'

let _ =
  let l = numbers 1 100 in
  square_of_sum l - sum_of_squares l
