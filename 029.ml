open Big_int

let uniq = function
  | [] -> []
  | l ->
      let l = List.sort compare l in
      List.fold_left (fun acc x ->
                        let acc = if List.hd acc = x then acc else x::acc in
                        acc) ([List.hd l]) (List.tl l)

let _ =
  let n = 100 in
  let rec iter i j acc =
    if j > n then acc
    else if i > n then iter 2 (j+1) acc
    else iter (i+1) j ((i,j)::acc)
  in
  let rec expo (x, y) = power_int_positive_int x y in
  List.length (uniq (List.map (fun x -> string_of_big_int (expo x)) (iter 2 2 [])))
