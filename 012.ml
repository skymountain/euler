let factors_number n =
  let rec div_completely n x acc =
    if n mod x = 0 then div_completely (n/x) x (acc+1)
    else (n, acc)
  in
  let div_completely n x = div_completely n x 0 in
  let rec iter i n acc =
    if i > n then acc
    else
      let (n, exp) = div_completely n i in
      let acc = acc * (exp + 1) in
      iter (i+1) n acc
  in
  iter 2 n 1
    
let min_triangle_over limit =
  let rec iter i curr =
    let factors_num = factors_number curr in
    if factors_num > limit then curr
    else iter (i+1) (curr+i)
  in
  iter 2 1

let _ = min_triangle_over 500
