let score tbl = 
    let countstr str =  
        let cnt = ref 0
        in
        String.iter (fun c -> cnt := (!cnt) + ((int_of_char c)-64)) str; (!cnt)
    in
    let (sum,_) = Array.fold_left (fun (sum,i) value -> (sum + ((i)*(countstr value)),i+1)) (0,1) tbl
    in
    sum;;
   
  let solve file =
  let in_channel = open_in file in
  let read_string() = Scanf.fscanf in_channel "%s@\"" (fun n->n) in
  let tbl = Array.make 5163 "" in
  let rec read_file i =
    match (read_string(),read_string()) with
    | name,"" -> tbl.(i) <- name;
    | name,_  -> tbl.(i) <- name; read_file (i+1);
                  
  in
  let _ = read_string() in
  read_file 0;
  Array.fast_sort (String.compare) tbl;
 score tbl;; 
