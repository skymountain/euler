let collatz_seq_length start =
  let rec iter x acc =
    let acc = succ acc in
    if x = 1 then acc
    else
      if x mod 2 = 0 then iter (x/2) acc
      else iter (3*x + 1) acc
  in
  iter start 0

let max_collatz_seq_legnth limit =
  let rec iter i (m, start) =
    if i >= limit then start
    else
      let n = collatz_seq_length i in
      let (m, start) = if m < n then (n, i) else (m, start) in
      iter (succ i) (m, start)
  in
  iter 1 (0, 0)

let _ = print_int (max_collatz_seq_legnth (1000*1000)); print_newline ()
