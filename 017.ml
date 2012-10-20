let numbers f t =
  if f > t then failwith "numbers";
  let rec iter i acc =
    let acc = i::acc in
    if i = f then acc
    else iter (i-1) acc
  in
  iter t []

let rec word_of = function
  | 0 -> ""
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | 10 -> "ten"
  | 11 -> "eleven"
  | 12 -> "twelve"
  | 13 -> "thirteen"
  | 14 -> "fourteen"
  | 15 -> "fifteen"
  | 16 -> "sixteen"
  | 17 -> "seventeen"
  | 18 -> "eighteen"
  | 19 -> "nineteen"
  | _ as n when n < 100 -> begin
      (match n / 10 with
       | 2 -> "twenty"
       | 3 -> "thirty"
       | 4 -> "forty"
       | 5 -> "fifty"
       | 6 -> "sixty"
       | 7 -> "seventy"
       | 8 -> "eighty"
       | 9 -> "ninety"
       | _ -> failwith "10 digit") ^
        word_of (n - (10 * (n/10)))
    end
  | _ as n -> begin
      let n100 = n / 100 in
      let str = (word_of n100) ^ "hundred" in
      str ^
        (if n mod 100 = 0 then "" else ("and" ^ word_of (n - n100*100)))
    end

let _ =
  let nums = numbers 1 999 in
  let sum =
    List.fold_left (+) 0
      (List.map (fun x -> String.length (word_of x)) nums)
  in
  sum + String.length "onethousand"
