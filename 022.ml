let name_score name =
  let char_score c = int_of_char c - int_of_char 'A' + 1 in
  let score = ref 0 in
  String.iter (fun c -> score := char_score c + !score) name;
  !score

let get_names cin =
  let rec iter acc =
    try
      let name = input_line cin in
      iter (name::acc)
    with
      End_of_file -> acc
  in
  iter []

let iteri f l =
  let rec iter i = function
    | [] -> ()
    | x::xs -> f i x; iter (i+1) xs
  in
  iter 0 l

let _ =
  let names = get_names stdin in
  let names = List.sort compare names in
  let l = ref [] in
  iteri (fun i name -> l := (i+1,name)::!l) names;
  List.fold_left (+) 0
    (List.map (fun (i,name) -> i * name_score name) !l)

