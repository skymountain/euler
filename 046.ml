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

let is_composite n =
  if n <= 1 then false
  else not (is_prime n)

let is_sqrt_number n =
  let sq = int_of_float (sqrt (float n)) in
  n = sq * sq

type 'a seq = Cons of 'a * (unit -> 'a seq)

let rec from n = Cons (n, fun () -> from (n+1))

let rec sift n (Cons (x, f)) =
    if (x mod n) = 0 then sift n (f ())
    else Cons (x, fun () -> sift n (f ()))

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f ())))

let primes = sieve (from 2);;

let is_CG n =
  let rec iter (Cons (h, t)) =
    let m = n - h in
    if m <= 0 then false
    else (m mod 2 = 0 && is_sqrt_number (m/2)) || iter (t ())
  in
  iter primes

let _ =
  let rec iter n =
    if is_composite n && not (is_CG n) then n
    else iter (n+2)
  in
  iter 9
