let sum_of_diagonals n =
  let rec iter i acc =
    if i > n then acc
    else
      let m1 = i * i in
      let m2 = m1 - (i - 1) in
      let m3 = m2 - (i - 1) in
      let m4 = m3 - (i - 1) in
      let acc = acc + m1 + m2 + m3 + m4 in
      iter (i+2) acc
  in
  iter 3 1

let _ = sum_of_diagonals 1001
