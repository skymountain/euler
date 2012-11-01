let digit x = int_of_float (log10 (float x)) + 1

let list_of_string s =
  let len = String.length s in
  let rec iter i acc =
    if i = len then acc
    else iter (i+1) (s.[i]::acc)
  in
  List.rev (iter 0 [])

let list_of_int x = list_of_string (string_of_int x)
let int_of_list l =
  let len = List.length l in
  let str = String.make len '\000' in
  ignore (List.fold_left (fun i c -> str.[i] <- c; i+1) 0 l);
  int_of_string str

let pandigital n digits =
  let rec iter i len acc =
    let ni = n * i in
    let len = len + digit ni in
    if len > digits then None
    else
      let acc = acc @ (list_of_int ni) in
      if len = digits then Some (i, acc)
      else iter (i+1) len acc
  in
  match iter 1 0 [] with
  | None -> None
  | Some (num, l) ->
      if num <> 1 && (List.length l = digits) &&
        fst (List.fold_left
               (fun (acc, expect) x ->
                  (acc && expect = (int_of_char x - int_of_char '0'), expect+1))
               (true, 1) (List.sort compare l)) then
          Some (int_of_list l)
      else
        None

let _ =
  let max_num = 999999999 / 3 in
  let digits = 9 in
  let rec iter i acc =
    if i > max_num then acc
    else begin
      let acc = match pandigital i digits with | Some n -> n::acc | None -> acc in
      iter (i+1) acc
    end
  in
  let l = iter 1 [] in
  List.fold_left max 0 l
