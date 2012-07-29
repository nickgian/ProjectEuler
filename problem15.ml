
let bfs start =
    let queue = Queue.create () in
    let hash = Hashtbl.create 400 in
    let doChildren lst parentpaths=
        List.iter (fun (x,y) ->
                     match (try Hashtbl.find hash (x,y) with Not_found -> -1 ) with
                         | -1 -> Queue.add (x,y) queue;
                                 Hashtbl.add hash (x,y) parentpaths;
                         | n  -> Hashtbl.replace hash (x,y) (parentpaths+n)
            ) lst;
    in
  let findChildren (x,y) =
    match x,y with
     | 20,20 -> []
     | 20,y -> [(20,y+1)]
     | x,20 -> [(x+1,20)]
     | x,y -> [(x+1,y);(x,y+1)]
  in
  let rec procnode node =
      let paths = Hashtbl.find hash node in
      match (findChildren node) with
        | [] -> Printf.printf "%d\n" paths
        | lst -> doChildren lst paths;
                  if ((Queue.is_empty queue) == true) then ()
                  else procnode (Queue.pop queue)
  in
  Hashtbl.add hash start 1;
  Queue.add start queue;
  procnode (Queue.pop queue)
;;
