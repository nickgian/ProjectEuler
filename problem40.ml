let int_pow a b = truncate ((float a) ** (float b));;

let digitize number =
  let rec aux num acc =
    match num with 
      | n when n<10 -> (n::acc)
      | n -> let digit = n mod 10 in
            aux (n/10) (digit::acc)
  in 
    aux number [];;


let rec digcount diglst digsum numsum digs acc =
  let nextnumsum = 9*(int_pow 10 (digs-1)) in
    match diglst with
      | [] -> acc
      | x::xs when ((digsum<x) && (x<(digsum+digs*nextnumsum))) ->
          let dig = x-digsum in
          let num = numsum+(dig / digs)+1 in
          let digno =  dig mod digs in
            begin
              match digno with
                | 0 -> digcount xs digsum numsum digs ((List.nth (digitize num) (digs-1))*acc)
                | digno -> digcount xs digsum numsum digs ((List.nth (digitize num) (digno-1))*acc)
            end
      | x::xs when (x>(digsum+digs*nextnumsum)) ->
          digcount diglst (digsum+digs*nextnumsum) (numsum+nextnumsum) (digs+1) acc
      | _ -> failwith "You have failed"
;;

let solve = digcount [100;1000;10000;100000;1000000] 9 9 2 1;;

