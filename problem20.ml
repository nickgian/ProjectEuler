let rec fact n =
    match n with
    | 1 -> unit_big_int
    | n -> mult_int_big_int n (fact ((n-1)));;
        
let solve = let num = fact 100 in
            let str = Big_int.string_of_big_int num in
            let cnt = ref 0 in
            String.iter (fun x -> cnt := ((!cnt) + (int_of_char x) - 48)) str;
            Printf.printf "%d\n" (!cnt);
