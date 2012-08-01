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

let solve n =
    let tbl = Array.make (n+5) 0 in
    let rec aux i acc=
        match i with
        | i when (n == i) -> acc
        | i -> let d = sumDivisors i in
                tbl.(i) <- d;
                match d with 
                 | d when ((d<i)&&(d>0)) -> if (i == tbl.(d)) then aux (i+1) (acc + i + d)  else aux (i+1) acc
                 | _ -> aux (i+1) acc   
    in
    aux 1 0
;;
