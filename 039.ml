let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let rec num_of_solutions p =
  let rec iter m n acc =
    if m > n then
      iter 1 (n+1) acc
    else if n > p/2 then acc
    else
      let l = p - m - n in
      let acc = acc + if m*m + n*n = l*l then 1 else 0 in
      iter (m+1) n acc
  in
  iter 1 1 0

let _ =
  let l = List.map (fun x -> (x, num_of_solutions x)) (numbers 1 1000) in
  let l = List.sort (fun (_, x) (_, y) -> compare y x) l in
  fst (List.hd l)
