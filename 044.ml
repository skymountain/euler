let pentagonal_th i = (i * (3 * i - 1)) / 2

let is_pentagonal_over n i =
  let rec iter i =
    let m = pentagonal_th i in
    if m = n then true
    else if m > n then false
    else iter (i+1)
  in
  iter (i+1)
  
let abs n = if n < 0 then -n else n

module IntSet = Set.Make(struct type t = int let compare = compare end)

let _ =
  let rec iter pents d i =
    let p = pentagonal_th i in
    if p - (pentagonal_th (i-1)) > d then d
    else
      let d = IntSet.fold
        (fun p' d ->
           let d' = abs (p - p') in
           if d' < d && IntSet.mem d' pents && is_pentagonal_over (p+p') i
           then d' else d)
        pents d
      in
      iter (IntSet.add p pents) d (i+1)
  in
  iter IntSet.empty max_int 1
