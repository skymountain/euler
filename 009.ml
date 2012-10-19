let calc sumval =
  let rec iter a b c =
    if a >= b then iter 0 (b+1) c
    else if b >= c then iter a 0 (c+1)
    else if c > 1000 then failwith "calc"
    else 
      let sum = a + b + c in
      if sum = sumval && a*a + b*b = c*c then a * b * c
      else iter (a+1) b c
  in
  iter 0 0 0

let _ = calc 1000
