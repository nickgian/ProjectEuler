let count =
    let rec aux num step res =
        match step with
          | 1002 -> res
          | step -> aux (num+4*step) (step+2) (res+4*num+10*step)
    in
    aux 1 2 1;;
