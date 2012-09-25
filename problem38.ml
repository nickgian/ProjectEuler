let int_pow a b = truncate ((float a) ** (float b));;

let digitize number =
  let rec aux num acc =
    match num with 
      | n when n<10 -> (n::acc)
      | n -> let digit = n mod 10 in
          aux (n/10) (digit::acc)
  in 
    aux number [];;

let undigitize lst =
  let len = List.length lst in
  let rec aux l fact acc =
    match l with 
      | [] -> acc 
      | x::xs -> aux xs (fact/10) (acc+x*fact)
  in
    aux lst (int_pow 10 (len - 1)) 0
;;

let pandigital lst =
  ((List.sort compare lst) = [1;2;3;4;5;6;7;8;9])
;;

let product num =
  let rec aux factor acc =
    match List.length acc with 
      | 9 -> if (pandigital acc) then (undigitize acc) else 0 
      | l when l > 9 -> 0
      | l -> aux (factor+1) (acc@(digitize (num*factor)))
  in
    aux 1 []
;;

let solve =
  let rec iterate i maxim =
    match i with 
      | 10000 -> maxim
      | i -> let concatenated = product i in
                iterate (i+1) (max maxim concatenated)
  in
    iterate 1 0
;;








