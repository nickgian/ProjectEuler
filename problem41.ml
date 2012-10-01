let int_pow a b = truncate ((float a) ** (float b));;


let isprime n =
    let bound = truncate (sqrt (float n)) in
    let rec aux i =
        if i > bound then true else 
            match (n mod i) with
              | 0 -> false
              | _ -> aux (i+1)
in
    aux 2
;;

let swap ind1 ind2 tbl =
    let temp = tbl.(ind1) in
      tbl.(ind1) <- tbl.(ind2);
      tbl.(ind2) <- temp 
;;

let rec reverse ind1 ind2 tbl =
    match (ind2-ind1) with 
        | 0 -> ()
        | 1 -> swap ind1 ind2 tbl
        | _ -> swap ind1 ind2 tbl; 
               reverse (ind1 + 1) (ind2 - 1) tbl
;;

let fact num =
  let rec aux i acc =
        match i with
            | i when (i=num) -> acc*num
            | i -> aux (i+1) (acc*i)
    in
    aux 2 1
;;

let permutations digits =
    let perm = Array.make digits 0 in 
    let k = ref 0 in 
    let l = ref 0 in 
    let lst = ref [] in
    let num = ref 0 in 
    let bound = fact digits in
    Array.iteri (fun ind _ -> perm.(ind) <- (ind+1)) perm; 
    for i = 2 to bound do
        k := (digits-2);
        while (perm.(!k) >= perm.((!k)+1)) do
            k := (!k) - 1; 
        done;
        l := (digits-1);
        while (perm.(!k) >= perm.(!l)) do
            l := (!l) - 1; 
        done;
        swap (!k) (!l) perm;
        reverse ((!k)+1) (digits-1) perm;
        num := 0;
        Array.iteri (fun ind value ->  (num:= (!num) + value*(int_pow 10 (digits-ind-1)))) perm;
        lst:=((!num)::(!lst));
    done;
    let rec iterate lst =
        match lst with 
        | [] -> 0
        | (x::xs) -> if (isprime x) then x else iterate xs
    in   
    iterate (!lst)
;;

let solve =
    let rec aux i =
        match permutations i with
            | 0 -> aux (i-1)
            | x -> x
    in
    aux 9
;;
