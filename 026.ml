let length_of_cycle n =
  let idx = Array.make (n-1) 0 in
  let rec iter i x =
    let x = x * 10 in
    if x < n then iter (i+1) x
    else
      let r = x mod n in
      if r = 0 then 0
      else
        if idx.(r-1) = 0 then (idx.(r-1) <- i; iter (i+1) r)
        else i - idx.(r-1)
  in
  iter 0 1
      
let nth_of_max = function
  | [] -> failwith "invalid argument"
  | x::xs ->
      let _, n, _ =
        List.fold_left (fun (m, n, i) x ->
                          let (m, n) = if x < m then (m, n) else (x, i) in
                          (m, n, i+1))
          (x, 0, 1) xs
      in
      n

let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let _ = 1 + nth_of_max (List.map length_of_cycle (numbers 1 999))
