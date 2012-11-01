let list_of_string s =
  let len = String.length s in
  let rec iter i acc =
    if i = len then acc
    else iter (i+1) (s.[i]::acc)
  in
  List.rev (iter 0 [])

let int_of_char c = int_of_char c - int_of_char '0'

let mult_pandigital m n digits =
  let list_of_int x = list_of_string (string_of_int x) in
  let l = (list_of_int m) @ (list_of_int n) @ (list_of_int (m*n)) in
  (List.length l = digits) &&
    fst (
      List.fold_left
        (fun (acc, expect) x -> (acc && expect = int_of_char x, expect+1))
        (true, 1) (List.sort compare l)
    )

let uniq = function
  | [] -> []
  | l ->
      let l = List.sort compare l in
      List.fold_left (fun acc x ->
                        let acc = if List.hd acc = x then acc else x::acc in
                        acc) ([List.hd l]) (List.tl l)

let sum = List.fold_left (+) 0

let _ =
  let max_num = 9999999 in
  let digits = 9 in
  let digit x = int_of_float (log10 (float x)) inxp
  let over_capacity m n =
    (digit m) + (digit n) + (digit (m*n)) > digits
  in
  let rec iter m n acc =
    if n > max_num then acc
    else if over_capacity m n then
      iter 1 (n+1) acc
    else
      let acc = if mult_pandigital m n digits then (m*n)::acc else acc in
      iter (m+1) n acc
  in
  let l = iter 1 1 [] in
  sum (uniq l)
