let rec mklist n lim acc =    
    match n with 
      | n when (n==lim) -> n::acc
      | n -> mklist (n-1) lim (n::acc)
;;

let doomed_first_try n = 
    let hash = Hashtbl.create 5000000 in
    let rec findseq n =
       (*Printf.printf "checking %d\n" n; *)
       match (try Hashtbl.find hash n with Not_found -> 0) with
        | 0 ->
                        begin
                            match (n land 1) with 
                                | 0 -> let value = ((findseq (n lsr 1))+1) in
                                        Hashtbl.add hash n value;
                                        value
                                | 1 -> let value = ((findseq ((n lsl 1) + n + 1)) + 1) in
                                        Hashtbl.add hash n value; 
                                        value
                                | _ -> failwith "Sense. This value makes none" 
                        end  
       | hashvalue ->  hashvalue
    in
    Hashtbl.add hash 1 1;
    Hashtbl.add hash 2 2;
    List.fold_left (fun (num,maxn) x -> 
        let mynew = findseq x in
          if (mynew>maxn) then (x,mynew) else (num,maxn)) (2,2) (mklist (n-1) 3 [])
;;
