open Big_int

let rec fact n =
  if n = 0 then big_int_of_int 1
  else mult_int_big_int n (fact (pred n))

let count_fact_word n =
  let fact_num = fact n in
  let fact_str = string_of_big_int fact_num in
  let l = ref [] in
  String.iter (fun c -> l := (int_of_char c - int_of_char '0')::!l) fact_str;
  List.fold_left (+) 0 !l

let _ = count_fact_word 100
