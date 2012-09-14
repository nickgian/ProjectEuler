(*only checking xy/zx and xy/yz*)

let checkNum x y = 
  let rec commonX z (num,denum) =
    match z with
     | 10 -> (num,denum)
     | z ->  
            if (10*x*z+y*z=10*z*y+y*x) then
            begin
                commonX (z+1) (num*y,denum*z);
            end
            else
                commonX (z+1) (num,denum)
  in
  let rec commonY z (num,denum) =
    match z with
     | 10 -> (num,denum)
     | z ->  
            if (10*x*z+y*z=10*x*y+z*x) then
            begin
                commonY (z+1) (num*x,denum*z);
            end
            else
                commonY (z+1) (num,denum)
  in
      let (num1,denum1) = commonX (y+1) (1,1) in
      let (num2,denum2) = commonY (x+1) (1,1) in
        (num1*num2,denum1*denum2)
;;

let rec gcd a b =
  match b with
    | 0 -> a
    | b -> gcd b (a mod b)
;;

let solve =
    let (num,denum) = (ref 1, ref 1) in
    for i = 1 to 9 do
        for j = 1 to 9 do
            let (n,d) = checkNum i j in
             num := (!num)*n;
             denum := (!denum)*d;
        done;
    done; 
    let gcdnd = gcd (!num) (!denum) in
    let result = (!denum) / gcdnd
    in
    result
;;    
    

