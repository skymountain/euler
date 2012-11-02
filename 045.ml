let triangle_th i = (i * (i + 1)) / 2
let pentagonal_th i = (i * (3*i - 1)) / 2
let hexagonal_th i = (i * (2 * i - 1))

let _ =
  let no = 3 in
  let rec iter_triangle acc ti pi hi =
    let p = pentagonal_th pi in
    let rec iter ti =
      let t = triangle_th ti in
      if t >= p then iter_hexagonal acc ti pi hi
      else iter (ti+1)
    in
    iter ti
  and iter_pentagonal acc ti pi hi =
    let h = hexagonal_th hi in
    let rec iter pi =
      let p = pentagonal_th pi in
      if p >= h then iter_triangle acc ti pi hi
      else iter (pi+1)
    in
    iter pi
  and iter_hexagonal acc ti pi hi =
    let t = triangle_th ti in
    let p = pentagonal_th pi in
    let h = hexagonal_th hi in
    let acc = acc + if t = p && h = p then 1 else 0 in
    if acc = no then t
    else iter_pentagonal acc ti pi (hi+1)
  in
  iter_hexagonal 0 1 1 1
    
