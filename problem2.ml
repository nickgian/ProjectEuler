let fib n = 
    let rec fibaux num acc1 acc2 acc3=
        match acc2 with
            | n when n > num -> acc3
            | acc2 when (acc2 mod 2 == 0) -> fibaux num acc2 (acc1+acc2) (acc3+acc2) 
            | _ -> fibaux num acc2 (acc1+acc2) (acc3)
    in
    fibaux n 1 2 0 ;;

