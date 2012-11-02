let is_prime n =
  let limit = (int_of_float (sqrt (float n) +. 1.)) in
  let rec iter i =
    if i > limit then true
    else if n mod i = 0 then false
    else iter (i+2)
  in
  if n = 1 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else iter 3

let circular_prime n =
  let s = string_of_int n in
  let len = String.length s in
  let rec next () =
    let rec iter i =
      if i = len - 1 then ()
      else
        let c = s.[i] in
        begin
          s.[i] <- s.[i+1];
          s.[i+1] <- c;
          iter (i+1)
        end
    in
    iter 0
  in
  let rec iter i =
    if i = len then true
    else if not (is_prime (int_of_string s)) then false
    else begin
      next ();
      iter (i+1) 
    end
  in
  iter 0
     
let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let _ =
  List.length (List.filter circular_prime (numbers 1 1000000))
