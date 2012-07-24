let rec sum num acc =
    match num with
        | 1001 -> acc
        | _ ->  if (((num mod 3) == 0) or ((num mod 5) == 0)) then sum (num+1) (acc+num)
                else sum (num+1) acc;;
