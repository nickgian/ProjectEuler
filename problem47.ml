let sieve bound =
  let bitmap = Array.make (bound+1) 0 in
  let root = bound / 2 in 
    for i = 2 to root do
      if (bitmap.(i) == 0) then 
        let j = ref (i*2) 
        in   
          while (!j <= bound) do
            bitmap.(!j) <- bitmap.(!j) + 1;
            j := !j + i; 
          done;
    done;
    bitmap
;;


let solve =
  let primes = sieve 1000000 in
  let rec consecutive n acc =
    match (primes.(n)) with
      | 4 -> if ((acc+1)=4) then (n-3) else consecutive (n+1) (acc+1)
      | _ -> consecutive (n+1) 0
  in 
    consecutive 2 0
;;
