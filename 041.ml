let is_prime n =
  let limit = (int_of_float (sqrt (float n) +. 1.)) in
  let rec iter i =
    if i > limit then true
    else if n mod i = 0 then false
    else iter (i+2)
  in
  if n = 1 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else iter 3

let conv c = int_of_char c - int_of_char '0'

let max_pandigital_prime l =
  let rec iter acc = function
    | [] -> if is_prime acc then Some acc else None
    | l -> begin
        let acc = acc * 10 in
        let r, _, _ =
          List.fold_left
            (fun (res, h, t) x -> match res with
             | Some _ -> (res, h, t)
             | None -> begin
                 (iter (acc + x) (h@t), h@[x], if t = [] then [] else List.tl t)
               end)
            (None, [], List.tl l) l
        in
        r
      end
  in
  iter 0 l

let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let _ =
  List.fold_left
    (fun acc x -> match acc with
     | Some _ -> acc | None -> max_pandigital_prime (List.rev (numbers 1 x)))
    None (List.rev (numbers 1 9))
