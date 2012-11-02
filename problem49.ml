let array_of_int n = Array.of_list (MathTools.digitize n)
let int_of_array n = MathTools.undigitize (Array.to_list n)

let solve =
  let primes = Primes.sieve 9999 in
    primes.(1487) <- false;
    let rec getPrimePerms num start_num acc =
      let perm = array_of_int num in
        match ( try Some (Permutations.nextPermutation perm) with Permutations.LastPermutation -> None ) with
          | None -> List.rev acc 
          | Some _ -> 
              let prm = (int_of_array perm) in
                if ((primes.(prm)) && ((prm - start_num) = 3330)) then 
                  (getPrimePerms prm prm (prm::acc))
                else getPrimePerms prm start_num acc
    in
    let rec check num acc  =
      if (num < 10000) then
        match primes.(num) with
          | true -> 
              let solution = getPrimePerms num num [num] in
                if (List.length solution > 2) then
                  solution
                else
                  check (num+1) acc
          | false -> check (num+1) acc
            else
              acc
    in
    match (check 1000 []) with
      | x::y::z::[] -> Printf.printf "%d%d%d\n" x y z
      | _ -> failwith "Not found\n"
;;

