open Big_int

let readmyint in_chan= 
  let in_channel = open_in in_chan in
  let read_string() = Scanf.fscanf in_channel "%s\n" (fun n->n) in
  let rec sum n acc =
   if (n==0) then Printf.printf "%s\n" (String.sub (Big_int.string_of_big_int acc) 0 10)
   else 
     begin
       let big = Big_int.big_int_of_string (read_string()) in
         sum (n-1) (Big_int.add_big_int acc big)
     end
 in
 sum 100 (Big_int.big_int_of_int 0)
;;
