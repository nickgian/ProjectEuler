open Euler;;

let valid d =
  let num i = ((int_of_baseTypes d.(i))*100 + (int_of_baseTypes d.(i+1))*10 + (int_of_baseTypes d.(i+2))) in 
    ((((num 1) mod 2) = 0) &&
     (((num 2) mod 3) = 0) &&
     (((num 3) mod 5) = 0) &&
     (((num 4) mod 7) = 0) &&
     (((num 5) mod 11) = 0) &&
     (((num 6) mod 13) = 0) &&
     (((num 7) mod 17) = 0))
;;

let solve = 
    let xs = [Int 0; Int 1; Int 2; Int 3; Int 4; Int 5; Int 6; Int 7; Int 8; Int 9] in
    let perm = Array.of_list xs in
    let digits = Array.length perm in
    let num = ref 0 in
    let sum = ref 0 in
    let rec aux() =
        match try Some (Permutations.nextPermutation perm) with Permutations.LastPermutation -> None with
            | Some _ -> Array.iteri (fun ind value ->  (num := (!num) + (int_of_baseTypes value)*(MathTools.int_pow 10 (digits-ind-1)))) perm;
                        if (valid perm) then sum := (!sum) + (!num);
                        num := 0;
                        aux ();
            | None ->  (!sum)
    in
        aux ()
;;

