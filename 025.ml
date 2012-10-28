open Big_int

let fib_over limit =
  let rec iter i prev1 prev2 =
    let curr = add_big_int prev1 prev2 in
    if le_big_int limit curr then i
    else iter (i+1) curr prev1
  in
  iter 3 (big_int_of_int 1) (big_int_of_int 1)

let _ = fib_over (big_int_of_string ("1" ^ String.make 999 '0'))
