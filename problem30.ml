let digitize x =
    let rec aux num acc =
        match num with 
          | n when n<10 -> (n::acc)
          | n -> aux (n/10) ((n mod 10)::acc)
    in aux x [];;

let pow5 a = a*a*a*a*a;;

let rec doSum x sum =
    match x with
    | [] -> sum
    | (x::xs) -> doSum xs ((pow5 x) + sum)
    ;;
    
let solve =
    let rec count i acc =
      match i with
        | 354294 -> acc
        | i ->  if ((doSum (digitize i) 0) = i) then count (i+1) (i+acc) else count (i+1) acc
    in
    count 100 0;;
