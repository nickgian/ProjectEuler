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


let permutations =
    let perm = Array.make 10 0
    in
    let k = ref 0
    in
    let l = ref 0
    in
    Array.iteri (fun ind _ -> perm.(ind) <- ind) perm; 
    for i = 2 to 1000000 do
        k := 8;
        while (perm.(!k) >= perm.((!k)+1)) do
            k := (!k) - 1;
        done;
        l := 9;
        while (perm.(!k) >= perm.(!l)) do
            l := (!l) - 1;
        done;
        swap (!k) (!l) perm;
        reverse ((!k)+1) 9 perm;
    done;
    Array.iter (fun value -> Printf.printf "%d" value) perm;
    Printf.printf "\n"
    ;;
