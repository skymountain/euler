type 'a seq = Cons of 'a * (unit -> 'a seq)

let rec from n = Cons (n, fun () -> from (n+1))

let rec sift n (Cons (x, f)) =
    if (x mod n) = 0 then sift n (f ())
    else Cons (x, fun () -> sift n (f ()))

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f ())))

let primes = sieve (from 2);;

module IntSet = Set.Make(struct type t = int let compare = compare end)

let is_truncatable n primes =
  let rec truncate_from f s acc =
    if s = "" then acc
    else
      let s' = f s in
      truncate_from f s' ((int_of_string s)::acc)
  in
  let s = string_of_int n in
  let left =
    truncate_from (fun s -> String.sub s 1 (String.length s - 1)) s [] in
  let right =
    truncate_from (fun s -> String.sub s 0 (String.length s - 1)) s [] in
  List.for_all (fun x -> IntSet.mem x primes) (left@right)

let collect_truncatable_primes n =
  let rec iter acc len primes next_primes =
    if len >= n then acc
    else
      match next_primes with
      | Cons (h, t) ->
          let primes = IntSet.add h primes in
          let (acc, len) =
            if is_truncatable h primes then (h::acc, len+1)
            else (acc, len)
          in
          iter acc len primes (t ())
  in
  iter [] 0 IntSet.empty primes

let _ =
  let sum = List.fold_left (+) 0 (collect_truncatable_primes 15) in
  sum - (2+3+5+7)
