let count =
    let hash = Hashtbl.create 10000 in
    let cnt = ref 0 in
    for a = 2 to 100 do
        for b = 2 to 100 do
            let x = ((float a) ** (float b)) in
            match (try Some (Hashtbl.find hash x) with Not_found -> None) with
              | Some a -> ()
              | None ->  cnt := (!cnt)+1; Hashtbl.add hash x 1;
        done;
    done;
    (!cnt)
;;

(*--------***Alternative Solution With List***--------*)

let remove_duplicates lst =
    let rec aux lst acc =
      match lst with 
        | [] -> acc
        | [x] -> aux [] (x::acc)
        | (x::y::rest) when (x = y) -> aux (y::rest) acc
        | (x::y::rest) -> aux (y::rest) (x::acc)
    in
    aux lst []
;;

let countalt =
    let l = ref [] in
    for a = 2 to 100 do
        for b = 2 to 100 do
            let x =  ((float a) ** (float b)) in
            l := x :: (!l);
        done;
    done;
    let sorted = List.sort compare (!l) in
    let final = remove_duplicates sorted in
    List.length final
;;
