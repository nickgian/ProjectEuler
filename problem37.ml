let int_pow a b = truncate ((float a) ** (float b));;

let digitize number =
  let rec aux num acc =
    match num with 
      | n when n<10 -> (n::acc)
      | n -> let digit = n mod 10 in
          if ((digit = 2) || (digit = 4) || (digit = 5)|| (digit = 6) || (digit = 8)) then []
          else aux (n/10) (digit::acc)
  in 
    aux number [];;

let undigitize lst =
  let len = List.length lst in
  let rec aux l fact acc =
    match l with 
      | [] -> acc 
      | x::xs -> aux xs (fact/10) (acc+x*fact)
  in
    aux lst (int_pow 10 (len - 1)) 0
;;

let rotations (number : int list)   =
  let len = List.length number in
  let rec aux lst i acc =
    match (i,lst) with
      | _,[] -> acc
      | i,_ when i = len -> acc
      | 0,(x::xs) -> aux (xs) (i+1) acc
      | i,(x::xs) -> aux (xs) (i+1) ((x::xs)::acc)
  in
    aux number 0 []
;;


let solve =
  let bound = 1000000.0 in
  let lim = truncate (bound) in      
  let bitmap = Array.make (lim+1) true in 
  let sieve (n : float) =
    bitmap.(1) <- false;
    let root = truncate (sqrt n) in   
      for i = 2 to root do
        if (bitmap.(i) == true) then 
          let j = ref (i*i) 
          in   
            while (!j <= lim) do
              bitmap.(!j) <- false;
              j := !j + i; 
            done;
      done;
  in
  let checkRotations num =
    if (num = []) then false
    else
      let rec aux lst =
        match lst with
          | [] -> true
          | (h::t) -> let number = (undigitize h) in
              if (bitmap.(number)) then aux t       
              else false
      in
        aux num
  in
  let counter = ref 0 in  
    sieve bound;
    let rec iterate i =
      match i with
        | 999991 -> (!counter)
        | i ->  (match (bitmap.(i)) with
                   | false -> iterate (i+2)
                   | true -> 
                       let number = digitize i in
                       let numbers = rotations number in
                       let revNumber = List.rev number in
                       let revNumbers = rotations revNumber in
                       let fixedRev = List.map (fun x -> List.rev x) revNumbers in
                         if ((checkRotations numbers) && (checkRotations fixedRev)) then
                           counter := (!counter) + i;
                         iterate (i+2))
    in
      iterate 13
;;
