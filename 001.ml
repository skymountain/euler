let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let rec sum = function [] -> 0 | x::xs -> x + sum xs
  
let _ = sum (List.filter (fun x -> x mod 5 = 0 or x mod 3 = 0) (numbers 1 999))
