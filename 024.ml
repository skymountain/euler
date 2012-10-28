let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

type ('a, 'b) either = Left of 'a | Right of 'b

let th_of_lexicographic_permutation l th =
  let rec iter i acc = function
    | [] -> failwith "don't reach here"
    | [x] -> if i = th then Left (x::acc) else Right (i+1)
    | l -> begin
        let r, _, _ =
          List.fold_left
            (fun (prev, h, t) x -> match prev with
             | Left r -> (Left r, h, t)
             | Right j -> begin
                 (iter j (x::acc) (h@t), h@[x], if t = [] then [] else List.tl t)
               end)
            (Right i, [], List.tl l) l
        in
        r
      end
  in
  match iter 1 [] l with
  | Left l -> List.rev l
  | Right _ -> failwith "over the specified number"

let _ = th_of_lexicographic_permutation (numbers 0 9) (1000*1000)
