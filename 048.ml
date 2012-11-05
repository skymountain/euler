let take_digit n digit =
  let n' = float n in
  let d = floor (log10 n') +. 1. in
  let digit' = float digit in
  let m = 10. ** digit' in
  if d > (float digit) then
    n - int_of_float (m *. (floor (n' /. m)))
  else
    n

let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let _ =
  let digit = 10 in
  let pow n m =
    let rec iter i acc =
      if i = 0 then acc
      else iter (i-1) (take_digit (acc * n) digit)
    in
    iter m 1
  in
  List.fold_left (fun x y -> take_digit (x+y) digit) 0
    (List.map (fun x -> pow x x) (numbers 1 1000))
