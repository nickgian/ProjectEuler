 let longDivision n =  
     let hash = Hashtbl.create n
     in   
     let rec aux num cnt =
         let remainder = num mod n in 
         match cnt,remainder with 
          | cnt,remainder when ((cnt==n)||(remainder==0)) -> 0 
          | cnt,remainder -> (match (try Some (Hashtbl.find hash remainder) with Not_found -> None) with 
                    | Some value -> cnt-value 
                    | None ->  Hashtbl.add hash remainder cnt; aux (remainder*10) (cnt+1))
    in   
    aux 1 0;;
          
 let solve n =  
     let rec aux i num result =
         match i with 
         | i when (i == n) -> num
         | i -> let cycle = longDivision i
                in
                if (cycle > result) then
                aux (i+1) i cycle
                else
                aux (i+1) num result
    in   
    aux 1 1 0;;
