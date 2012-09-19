let solve =
    let facts = Array.make 10 1 in
      for i = 1 to 9 do
        facts.(i) <- facts.(i-1)*i;
      done;
      let checkFacts number =
        let rec digitize num acc =
          match num with 
            | n when n<10 -> (n::acc)
            | n ->  digitize (n/10) ((n mod 10)::acc)
        in 
          if ((List.fold_left (fun sum x -> (sum + facts.(x))) 0 (digitize number [])) = number) then number
          else 0
      in
      let sum = ref 0 in
        for i = 10 to (7*facts.(9)) do
          sum := (!sum) + (checkFacts i)
        done;
        (!sum)
;; 
