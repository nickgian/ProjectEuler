open Euler;;

let pentNum n = (n * (3*n - 1)) lsr 1
;;

let solve =
  let mem = Array.make 3502 0 in
  let dif = ref max_int in
    for i = 1 to 3501 do
      mem.(i) <- (pentNum i);
    done;
    let bound = 2500 in
      for i = 1 to bound do
        let rec iter j =
          match j with
            | 0 -> ()
            | j -> let pi = mem.(i) in
              let pj = mem.(j) in
              let ps = pi+pj in
              let pd = pi-pj in
                if (((ArrayTools.bsearch (i+1) bound ps mem) != -1) && ((ArrayTools.bsearch 1 i pd mem)) != -1)
                then dif := min (!dif) pd
                else iter (j-1);
        in
          iter (i-1);
      done;
      (!dif)
;;
