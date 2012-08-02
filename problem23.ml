let int_pow a b = truncate ((float a) ** (float b))

let sumDivisors num =
  let lim = num/2  in   
  let rec divide n divisor power =
    match (n mod divisor) with 
      | 0 -> divide (n/divisor) divisor (power+1) 
      | _ -> (power,n)
  in    
  let rec divaux n divisor acc =
    if (n < 2) then (acc-num) 
    else 
      begin
        let (power,res) = divide n divisor 0 in 
          match divisor with 
            | d when (d==(lim+1)) -> (acc-num) 
            | d -> divaux res (divisor+1) (acc*(((int_pow divisor (power+1))-1)/(divisor-1))) 
      end  
  in
  divaux num 2 1  
;;

let solve =
    let tbl = Array.make 28123 false
    in
    let rec sum lst acc =
    match lst with
      | [] -> List.flatten acc
      | (h::t) -> sum t ((List.fold_left (fun lst x -> (h+x)::lst) [] (h::t))::acc)
    in
    let rec aux i acc=
        match i with
        | i when (i == 28123) -> List.rev acc
        | i -> let d = sumDivisors i in
                if (d > i) then aux (i+1) (i::acc) else aux (i+1) acc
    in
    let allAbundant = sum (aux 1 []) []
    in
    List.iter (fun a -> if (a < 28123) then tbl.(a) <- true) allAbundant;
    let counter = ref 0
    in
    Array.iteri (fun ind value -> if (value == false) then counter := (!counter) + ind ) tbl; 
    (!counter);
;;
