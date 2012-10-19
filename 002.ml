let fib limit =
  let rec iter = function
    | (x::y::_) as l ->
        let next = x + y in
        if next > limit then l
        else iter (next::l)
    | _ -> failwith "fib"
  in
  if limit < 1 then []
  else iter [1;1]

let sum = List.fold_left (+) 0
let even = List.filter (fun x -> x mod 2 = 0)

let _ = sum (even (fib (4*1000*1000)))
