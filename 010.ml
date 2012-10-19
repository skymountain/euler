type 'a seq = Cons of 'a * (unit -> 'a seq)

let rec from n = Cons (n, fun () -> from (n+1))

let rec sift n (Cons (x, f)) =
    if (x mod n) = 0 then sift n (f ())
    else Cons (x, fun () -> sift n (f ()))

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f ())))

let primes = sieve (from 2)

let sum_primes limit =
  let rec iter acc = function Cons (h, t) ->
    if h < limit then iter (acc+h) (t ())
    else acc
  in
  iter 0 primes

let _ = sum_primes (2*1000*1000)
