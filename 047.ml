let num_of_prime_divisors_is_equal n num =
  let rec div_if_possible n m =
    if n mod m = 0 then div_if_possible (n/m) m
    else n
  in
  let rec iter i m acc =
    if i > n/2 + 1 then acc = num
    else
      let acc = acc + if m mod i = 0 then 1 else 0 in
      iter (i+1) (div_if_possible m i) acc
  in
  iter 2 n 0

let _ =
  let c = 4 in
  let rec iter i count =
    if num_of_prime_divisors_is_equal i c then
      let count = count + 1 in
      if count = c then i
      else iter (i+1) count
    else
      iter (i+1) 0
  in
  (iter 1 0) - (c-1)
