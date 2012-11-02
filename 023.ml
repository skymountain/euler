let sum_of_divisors n =
  let rec iter i sum =
    if i > n/2 then sum
    else
      let sum = sum + if n mod i = 0 then i else 0 in
      iter (i+1) sum
  in
  iter 1 0

let is_abundant x = sum_of_divisors x > x

let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let uniq = function
  | [] -> []
  | l ->
      let l = List.sort compare l in
      List.fold_left (fun acc x ->
                        let acc = if List.hd acc = x then acc else x::acc in
                        acc) [List.hd l] (List.tl l)

let sum = List.fold_left (+) 0

let _ =
  let max_num = 28123 in
  let nums = numbers 1 max_num in
  let abundants = List.filter is_abundant nums in
  let sums = fst (
    List.fold_left
      (fun (acc, abundants) x ->
         (List.fold_left (fun acc y -> (x+y)::acc) acc (x::abundants)), x::abundants)
      ([], []) abundants)
  in
  let sums = uniq (List.filter (fun x -> x <= max_num) sums) in
  sum nums - sum sums
