let (+) = Big_int.add_big_int
let ( ** ) = Big_int.power_int_positive_int
let ( mod ) = Big_int.mod_big_int
let zero = Big_int.zero_big_int
let tenbillions = Big_int.big_int_of_int 10000000000

let print_big_int big = 
  Printf.printf "%s\n" (Big_int.string_of_big_int big)

let solve =
  let rec sumUp i acc =
    match i with 
      | 1001 -> acc
      | i -> sumUp (succ i) (acc+(i**i))
  in
  let num = sumUp 1 zero in
   print_big_int (num mod tenbillions)
;;
