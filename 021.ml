let sum_of_divisors n =
  let rec iter i sum =
    if i > n/2 then sum
    else
      let sum = sum + if n mod i = 0 then i else 0 in
      iter (i+1) sum
  in
  iter 1 0

let amicable_numbers limit =
  let arr = Array.make limit 0 in
  let rec iter i acc =
    if i >= limit then acc
    else
      let sum = sum_of_divisors i in
      arr.(i) <- sum;
      let acc =
        if sum < i && arr.(sum) = i then sum::i::acc
        else acc
      in
      iter (i+1) acc
  in
  iter 0 []

let _ = List.fold_left (+) 0 (amicable_numbers (10*1000))
