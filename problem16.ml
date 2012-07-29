let solve = 
    let power = Big_int.power_int_positive_int 2 1000 in
    let str = Big_int.string_of_big_int power in
    let cnt = ref 0 in
    String.iter (fun x -> cnt:=((!cnt)+(int_of_char x)-48)) str;
    Printf.printf "%d\n" (!cnt);
