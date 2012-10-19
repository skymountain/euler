(* By isle4fp *)

type 'a seq = Cons of 'a * (unit -> 'a seq)

let rec from n = Cons (n, fun () -> from (n+1))

let rec sift n (Cons (x, f)) =
    if (x mod n) = 0 then sift n (f ())
    else Cons (x, fun () -> sift n (f ()))

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f ())))

let primes = sieve (from 2);;

let rec nthseq n (Cons (x, f)) =
    if n = 1 then x
    else nthseq (n-1) (f())
    
let _ = nthseq 10001 primes
