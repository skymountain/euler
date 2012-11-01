let word_value s =
  let sum = ref 0 in begin
    String.iter (fun c -> sum := !sum + (1 + int_of_char c - int_of_char 'A')) s;
    !sum
  end

let is_triangle_number n =
  let m = 2 * n in
  let m' = float m in
  let rec iter i =
    if i > int_of_float (sqrt m') + 1then false
    else if i*(i+1) = m then true
    else iter (i+1)
  in
  iter 1

let is_triangle_word s = is_triangle_number (word_value s)

let _ =
  let s = begin
    let cin = open_in "042.txt" in
    let s = input_line cin in
    close_in cin;
    s
  end
  in
  let strs =
    Str.split (Str.regexp "\",\"")
      (String.sub s 1 (String.length s - 2))
  in
  List.length (List.filter is_triangle_word strs)
