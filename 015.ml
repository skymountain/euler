let matrix m n seed =
  let tmp = Array.make n seed in
  let matrix = Array.make m tmp in
  Array.iteri
    (fun i _ -> if i <> 0 then matrix.(i) <- Array.make n seed) matrix;
  matrix

let routes m n =
  let m = matrix m n 0 in
  Array.iteri
    (fun i a -> Array.iteri
       (fun j _ ->
          let routes =
            (if i > 0 then m.(i-1).(j) else 0) +
              (if j > 0 then m.(i).(j-1) else 0)
          in
          m.(i).(j) <- max routes 1) a) m;
  m

let _ = (routes 21 21).(20).(20)
