let is_palindromic n =
  let str = string_of_int n in
  let len = String.length str in
  let rec iter i =
    if i >= len then true
    else str.[i] = str.[len-i-1] && iter (i+1)
  in
  iter 0

let _ =
  let rec iter i j palin =
    if i > 999 then
      if j > 999 then palin
      else iter 100 (j+1) palin
    else
      let candidate = i * j in
      let palin =
        if is_palindromic candidate then max candidate palin
        else palin
      in
      iter (i+1) j palin
  in
  iter 100 100 0
        

