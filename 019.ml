let leap_year year = year mod 4 = 0 && (if year mod 100 = 0 then year mod 400 = 0 else true)

let number_of_days year month =
  match month with
  | 3 | 5 | 8 | 10 -> 30
  | 1 -> if leap_year year then 29 else 28
  | _ -> 31

let count f t init_day target =
  let rec iter_month y m day acc =
    if m > 11 then (acc, day)
    else begin
      let acc = acc + if day = target then 1 else 0 in
      let day = (day + number_of_days y m) mod 7 in
      iter_month y (m+1) day acc
    end
  in
  let rec iter_year y day acc =
    if y > t then acc
    else
      let (acc, day) =  iter_month y 0 day acc in
      iter_year (y+1) day acc
  in
  iter_year f init_day 0

let _ = count 1901 2000 2 0
