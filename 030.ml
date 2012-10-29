let pred n power =
  let power = float_of_int power in
  let s = string_of_int n in
  let len = String.length s in
  let rec iter i acc =
    if i = len then acc
    else
      let d = int_of_char s.[i] - int_of_char '0' in
      let d = int_of_float (float_of_int d ** power) in
      iter (i+1) (acc + d)
  in
  n = iter 0 0

let _ =
  let rec iter i acc =
    if i > 354294 then acc
    else
      let acc = if pred i 5 then i::acc else acc in
      iter (i+1) acc
  in
  List.fold_left (+) 0 (iter 2 [])
