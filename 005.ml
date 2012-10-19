let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let mult = List.fold_left ( * ) 1

let min_evenly_divisible f t =
  let l = numbers f t in
  let l = List.fold_left
    (fun acc x ->
       let r =
         List.fold_left
           (fun acc' x' -> if acc' mod x' = 0 then acc' / x' else acc')
           x acc
       in
       if r = 0 then acc else r::acc)
    [] l
  in
  mult l

let _ = min_evenly_divisible 1 20
