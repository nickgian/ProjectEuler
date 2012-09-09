
let rec mklist num acc  =
    match num with
    | 499 -> 499::acc
    | _ -> mklist (num+1) (num::acc)



let rec multone lst b = 
    match lst with
        | x::xs when (x < b) -> if ((2*x*b -2000*(x+b) + 1000*1000) == 0) then Printf.printf "a=%d b=%d\n" x b
                                else  (multone xs b) 
        | _                  -> ()
;;

let mult a b = List.iter (multone a) b;; 

let solve = mult (List.rev (mklist 1 [])) (List.rev (mklist 290 []));;

