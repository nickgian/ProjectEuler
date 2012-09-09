    let rec mklist num acc  =
        match num with
        | 499 -> 499::acc
        | _ -> mklist (num+1) (num::acc)
    
    let rec mult' la lb listA listB (a,b) =
    match la,lb with
    | [],[] -> (a,b)
    | [],(y::lb) -> mult' listA lb listA listB (a,b) 
    | x::la,y::lb when x < y -> if ((2*x*y - 2000*(x+y) + 1000*1000) == 0) then mult' [] [] listA listB (x,y) else mult' la (y::lb) listA listB (a,b)
    | x::la,y::lb when x >= y -> mult' la (y::lb) listA listB (a,b);;
 
    let solve' = let listA = List.rev (mklist 1 [])
                in
                let listB = List.rev (mklist 290 [])
                in
                mult' listA listB listA listB (0,0);;
