let fact n =
  let rec iter i acc =
    if i = 0 then acc
    else iter (i-1) (i*acc)
  in
  iter n 1

let curious n =
  let str = string_of_int n in
  let len = String.length str in
  let rec iter i acc =
    if i = len then acc
    else
      let acc = acc + fact (int_of_char str.[i] - int_of_char '0') in
      iter (i+1) acc
  in
  n = iter 0 0

let _ =
  let rec iter i acc =
    if i > 2540160 then acc
    else
      let acc = if curious i then (i::acc) else acc in
      iter (i+1) acc
  in
  List.fold_left (+) 0 (iter 3 [])
