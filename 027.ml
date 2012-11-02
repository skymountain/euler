let is_prime n =
  let limit = (int_of_float (sqrt (float n) +. 1.)) in
  let rec iter i =
    if i > limit then true
    else if n mod i = 0 then false
    else iter (i+2)
  in
  if n <= 0 then false
  else if n = 1 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else iter 3

let num_of_primes a b =
  let formula n = n*n + a*n + b in
  let rec iter i =
    if is_prime (formula i) then iter (i+1)
    else i
  in
  iter 0

let _ =
  let rec iter p m a b =
    if b >= 1000 then p
    else if a >= 1000 then iter p m (-999) (b+1)
    else
      let m' = num_of_primes a b in
      let p, m = if m' > m then a*b, m' else p, m in
      iter p m (a+1) b
  in
  iter min_int 0 (-999) (-999)
