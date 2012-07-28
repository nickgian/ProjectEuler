let sum (n : float) = 1.0/.2.0 *. (n +. 1.0) *. n
;;         

let numDivisors num =
  let lim = truncate (sqrt (float num))  in  
  let rec divide n divisor power =
    match (n mod divisor) with 
      | 0 -> divide (n/divisor) divisor (power+1) 
      | _ -> (power,n)
  in    
  let rec divaux n divisor fact =
    if (n < 2) then fact 
    else
      begin
        let (power,res) = divide n divisor 0 in
          match divisor with 
            | d when (d==(lim+1)) -> if (n>1) then (fact*2) else (fact)
            | d -> divaux res (divisor+1) (fact*(power+1)) 
      end  
  in
  divaux num 2 1  
;;

let rec brute n =
    let j = truncate (sum (float n)) in
    let res = ((numDivisors j)>500) in
    match res with
      | false -> brute (n+1)
      | true ->  ((sum (float n)),n);;
;;
