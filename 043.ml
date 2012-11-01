let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let zip l1 l2 =
  let rec iter acc = function
    | ([], _) | (_, []) -> acc
    | (x::xs, y::ys) -> iter ((x,y)::acc) (xs, ys)
  in
  List.rev (iter [] (l1,l2))

let has_div_prop n =
  let l = [(1,2);(2,3);(3,5);(4,7);(5,11);(6,13);(7,17)] in
  let s = string_of_int n in
  let s = (String.make (10 - String.length s) '0') ^ s in
  List.for_all
    (fun (idx, prime) -> (int_of_string (String.sub s idx 3)) mod prime = 0) l

let pandigital_div_prop_list l =
  let rec iter acc n = function
    | [] -> if has_div_prop n then n::acc else acc
    | l -> begin
        let n = n * 10 in
        let r, _, _ =
          List.fold_left
            (fun (acc, h, t) x ->
               (iter acc (n + x) (h@t), h@[x], if t = [] then [] else List.tl t))
            (acc, [], List.tl l) l
        in
        r
      end
  in
  iter [] 0 l

let _ =
  let l = pandigital_div_prop_list (numbers 0 9) in
  List.fold_left (+) 0 l
