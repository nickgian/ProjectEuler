let rec exploreLstOf (lst : int list list) prev acc =
  match lst,prev with 
    | [num]::tl, [dist] -> let newprev = (List.rev ((num+dist)::acc)) in
                           begin
                             match tl,newprev with 
                               | [], dists -> let findmax lst = List.fold_left ( fun maxval distance -> if (maxval>distance) then maxval else distance) 0 lst in
                                                findmax dists
                               | (((num1)::nums)::t), ((dist1)::dists) -> exploreLstOf (nums::t) (dist1::dists) ((num1+dist1)::[]) 
                               | _,_ -> failwith "Reached unreachable point"
                           end
    | (num::nums)::tl, (dist1::dist2::dists) -> let d1=num+dist1 in
                                             let d2=num+dist2 in
                                             let max= if (d1>d2) then d1 else d2 in
                                               exploreLstOf (nums::tl) (dist2::dists) ((max)::acc)  
    | _,_ -> failwith "Reached unreachable point"

let solve file =
  let in_channel = open_in file in
  let read_int () = Scanf.fscanf in_channel "%d " (fun n->n) in
  let read_intln () = Scanf.fscanf in_channel "%d\n" (fun n->n) in
  let rec read cnt start cnt2 acc acc2 =
    if (cnt2==0) then List.rev acc2 
    else
      match cnt with 
        | 1   ->  let num = read_intln() in
                  let newcnt = start+1 in
                    read newcnt newcnt (cnt2-1) [] ((List.rev (num::acc))::acc2)
        | cnt ->  let num = read_int() in
                    read (cnt-1) start cnt2 (num::acc) acc2
  in
  let size = read_intln() in
  let lst = (read 1 1 size [] []) in
    List.iter ( fun x -> (List.iter (fun y -> Printf.printf " %d" y) x; Printf.printf "\n")) lst;
    match lst with 
      | [x]::[x1;x2]::xs -> exploreLstOf ([x2]::xs) ([x]) ([x1+x]) 
      | _ -> failwith "Reached unreachable point"
;;

