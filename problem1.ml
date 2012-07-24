open Printf

let rec sum num acc =
    match num with
        | 1001 -> acc
        | _ ->  if (((num mod 3) == 0) or ((num mod 5) == 0)) then sum (num+1) (acc+num)
                else sum (num+1) acc;;

let rec sum3 num acc =
    match num with 
        | n when n > 1000 -> acc
        | _ -> printf "%d\n" num; sum3 (num+3) (acc+num);;

let rec sum5 num acc =
    match num with
        | n when n > 1000 -> acc
        | _ -> printf "%d\n" num; sum5 (num+5) (acc+num);;
        
let rec rem15 num acc =
    match num with
        | n when n > 1000 -> acc
        | _ -> printf "%d\n" num; rem15 (num+15) (acc+num);;
        
let sumpro = (sum3 0 0) + (sum5 0 0) - (rem15 0 0);;
