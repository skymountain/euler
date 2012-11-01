let conv c = int_of_char c - int_of_char '0'

let _ =
  let max_th = 1000000 in
  let buf = Buffer.create max_th in
  let rec iter i =
    if Buffer.length buf >= max_th then ()
    else begin
      Buffer.add_string buf (string_of_int i);
      iter (i+1)
    end
  in
  iter 1;
  List.fold_left ( * ) 1
    (List.map conv
       (List.map (Buffer.nth buf)
          [0; 9; 99; 999; 9999; 99999; 999999]))
