let _ =
  let str =
    Big_int.string_of_big_int (Big_int.power_int_positive_int 2 1000)
  in
  let l = ref [] in
  String.iter (fun c -> l := (int_of_char c - int_of_char '0')::!l) str;
  List.fold_left (+) 0 !l
