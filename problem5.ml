let rec mklist num = 
    match num with
        | 21 -> []
        | _ -> num :: mklist (num + 1);;


let rec lcm n xs = 
    let flag = (List.fold_left (fun value x -> ((n mod x)==0)&&value) true xs)
    in
    if (flag) then n else lcm (n+20) xs


let solve = lcm 20 (mklist 11);;

