let is_palindromic str =
  let len = String.length str in
  let rec iter i =
    if i >= len then true
    else str.[i] = str.[len-i-1] && iter (i+1)
  in
  iter 0

let log x y = (log10 y) /. (log10 x)

let to_binary n =
  let len = int_of_float (floor (log 2. (float_of_int n) +. 1.)) in
  let str = String.make len '0' in
  let rec iter n i =
    if n = 0 then ()
    else begin
      if n mod 2 = 1 then str.[len-i-1] <- '1';
      iter (n/2) (i+1)
    end
  in
  iter n 0;
  str

let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let _ =
  let l = numbers 1 (1000*1000 - 1) in
  let pred n =
    is_palindromic (string_of_int n) &&
      is_palindromic (to_binary n)
  in
  let l = List.filter pred l in
  List.fold_left (+) 0 l
