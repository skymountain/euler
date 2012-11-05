type 'a seq = Cons of 'a * (unit -> 'a seq)

let rec from n = Cons (n, fun () -> from (n+1))

let rec sift n (Cons (x, f)) =
    if (x mod n) = 0 then sift n (f ())
    else Cons (x, fun () -> sift n (f ()))

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f ())))

let primes = sieve (from 2);;

let list_of_string s =
  let len = String.length s in
  let rec iter i acc =
    if i = len then acc
    else iter (i+1) (s.[i]::acc)
  in
  List.sort compare (iter 0 [])

let normalize_int n =
  let rec collect n acc =
    if n = 0 then acc
    else
      let m = n / 10 in
      collect m ((n - m*10)::acc)
  in
  let rec compose l i acc = match l with
    | [] -> acc
    | x::xs -> compose xs (i+.1.) (acc + x * int_of_float (10. ** i))
  in
  compose (List.sort compare (collect n [])) 0. 0

let choose n l =
  let rec iter n l =
    if n = 1 then
      List.fold_left (fun acc x -> [x]::acc) [] l
    else
      let _, _, acc =
        List.fold_left
          (fun (l, len, acc) x ->
             if len < n-1 or len = 0 then
               ([], 0, acc)
             else
               let acc = (List.map (fun l -> x::l) (iter (n-1) l))@acc in
               (List.tl l, len - 1, acc))
          (List.tl l, List.length l - 1, []) l
      in
      acc
  in
  if List.length l < n then failwith "choose"
  else if n = 0 then []
  else iter n l

module IntMap = Map.Make(struct type t = int let compare = compare end)

let _ =
  let primes =
    let rec iter (Cons (h, t)) acc =
      if h <= 999 then iter (t ()) acc
      else if h >= 10000 then acc
      else iter (t ()) (h::acc)
    in
    iter primes []
  in
  let rec iter primes map = match primes with
    | [] -> map
    | p::ps ->
        let p' = normalize_int p in
        let l = try IntMap.find p' map with Not_found -> [] in
        iter ps (IntMap.add p' (p::l) map)
  in
  let l =
    IntMap.fold (fun _ l acc -> l::acc)
      (iter primes IntMap.empty) []
  in
  let l = List.filter (fun l -> List.length l >= 3) l in
  let l = List.fold_left
    (fun acc l -> (List.sort compare (choose 3 l))@acc) [] l
  in
  List.filter (fun [x;y;z] -> x - y = y - z) l

