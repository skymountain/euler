let collect coins n =
  let rec iter l n =
    if n = 0 then 1
    else match l with
    | [] -> 0
    | x::xs when x > n -> iter xs n
    | x::xs -> iter (x::xs) (n-x) + iter xs n
  in
  iter coins n

let _ = collect [200; 100; 50; 20; 10; 5; 2; 1] 200
