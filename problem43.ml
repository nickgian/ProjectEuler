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


let int_pow a b = truncate ((float a) ** (float b));;

let fact num =
  let rec aux i acc =
    match i with
      | i when (i=num) -> acc*num
      | i -> aux (i+1) (acc*i)
  in
    aux 2 1
;;

let valid d =
  let num i = (d.(i)*100+d.(i+1)*10+d.(i+2)) in 
    ((((num 1) mod 2) = 0) &&
     (((num 2) mod 3) = 0) &&
     (((num 3) mod 5) = 0) &&
     (((num 4) mod 7) = 0) &&
     (((num 5) mod 11) = 0) &&
     (((num 6) mod 13) = 0) &&
     (((num 7) mod 17) = 0))
;;



let permutations digits =
  let perm = Array.make digits 0 in 
  let k = ref 0 in 
  let l = ref 0 in 
  let sum = ref 0 in
  let num = ref 0 in 
  let bound = fact digits in
    Array.iteri (fun ind _ -> perm.(ind) <- ind) perm; 
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
      if (valid perm) then sum:=(!sum)+(!num);
    done;
    (!sum)
;;

let solve = permutations 10
