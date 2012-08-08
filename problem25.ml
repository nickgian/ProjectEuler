let fib = 
    let rec fibaux num acc1 acc2 =
      match num with
        | num when num > 4000 -> 
            (match String.length (Big_int.string_of_big_int acc2) with
                | 1000 -> num
                | _ -> fibaux (num+1) acc2 (Big_int.add_big_int acc1 acc2))
        | num -> fibaux (num+1) acc2 (Big_int.add_big_int acc1 acc2)
    
    in
        fibaux 3 Big_int.unit_big_int (Big_int.succ_big_int (Big_int.unit_big_int))   ;;
