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
    let xs = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let perm = Array.of_list xs in
    let digits = Array.length perm in
    let num = ref 0 in
    let sum = ref 0 in
    let rec aux() =
        match try Some (Permutations.nextPermutation perm) with Permutations.LastPermutation -> None with
            | Some _ -> Array.iteri (fun ind value ->  (num := (!num) + (value)*(MathTools.int_pow 10 (digits-ind-1)))) perm;
                        if (valid perm) then sum := (!sum) + (!num);
                        num := 0;
                        aux ();
            | None ->  (!sum)
    in
        aux ()
;;

