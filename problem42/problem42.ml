(*solve n^2+n-2Tn *)
let introot c =
    let d = 1 + 4*c in
    let sqrtd = sqrt (float d) in
    let s1 = (-1. -. sqrtd) /. 2. in
    let s2 = (-1. +. sqrtd) /. 2. in
    let trs1 = truncate s1 in
    let trs2 = truncate s2 in
    if (((s1 = (float trs1)) && (trs1 > 0))||((s2 = (float trs2)) && (trs2 > 0))) then true
    else false
;;

let countstr str =  
    let cnt = ref 0
    in
      String.iter (fun c -> cnt := (!cnt) + ((int_of_char c)-64)) str; 
      (!cnt)
;;

let score file =
  let in_channel = open_in file in
  let read_string() = Scanf.fscanf in_channel "%s@\"" (fun n->n) in
  let rec read_file cnt =
    match (read_string(),read_string()) with
      | name,"" -> if (introot (2*(countstr name))) then (cnt+1)
                   else cnt;
      | name,_  -> if (introot (2*(countstr name))) then read_file (cnt+1)
                   else read_file cnt;
  in
  let _ = read_string() in
    read_file 0
;;

let solve = score "words.txt"
;;
