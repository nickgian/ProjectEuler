let main =
  let cnt = ref 0 in
  let bc = Array.make_matrix 101 101 Z.zero in
    for i = 0 to 100 do
      bc.(i).(0) <- Z.one;
    done;
    for n = 1 to 100 do
      for k = 1 to 100 do
        bc.(n).(k) <- Z.(+) bc.(n-1).(k-1) bc.(n-1).(k);
        if (bc.(n).(k) > (Z.(~$) 1000000)) then incr cnt;
      done;
    done;
    Printf.printf "%d\n" !cnt;
    !cnt
;;

