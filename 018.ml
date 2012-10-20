let make_triangle cin =
  let reg = Str.regexp "[ \t]+" in
  let rec iter i =
    try
      let str = input_line cin in
      let arr = iter (i+1) in
      arr.(i) <- Array.of_list (List.map (fun x -> try int_of_string x with Failure "int_of_string" -> (print_string x; print_string " "; 0)) (Str.split reg str));
      arr
    with
      End_of_file -> Array.make i (Array.make 0 0)
  in
  iter 0

let max_route t =
  let rec iter i =
    if i >= 0 then begin
      let row = t.(i) in
      let row_len = Array.length row in
      let next_row = t.(i+1) in
      let rec iter_row j =
        if j <> row_len then begin
          row.(j) <- row.(j) + (max next_row.(j) next_row.(j+1));
          iter_row (j+1)
        end
      in
      iter_row 0;
      iter (i-1)
    end
  in
  if Array.length t <> 1 then 
    iter (Array.length t - 2)

let _ =
  let t = make_triangle stdin in begin
    max_route t; t.(0).(0)
  end
  
