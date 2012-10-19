let max_prime_factor n =
  let rec div_completely n x =
    if n mod x = 0 then div_completely (n/x) x
    else n
  in
  let rec iter i n max =
    if i > n then max
    else
      let max = if n mod i = 0 then i else max in
      iter (i+1) (div_completely n i) max
  in
  iter 2 n 0

let _ = max_prime_factor 600851475143
