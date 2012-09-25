let solve =
  let p = Array.make 1001 0 in
  let imax = ref 0 in
  let vmax = ref 0 in
  let iterateb a =
    let rec aux b =
      let floatc = (sqrt (float (a*a + b*b))) in
      let c = truncate floatc in
        match (floatc) with
          | n when (n>(1000.0 -. (float a) -. (float b))) -> ()
          | n when (n = (float c)) -> p.(a+b+c) <- p.(a+b+c) + 1; aux (b+1)
          | n -> aux (b+1)
    in
      aux 1
  in
  let rec iteratea a =
    match a with
      | 500 -> ()
      | a -> iterateb a; iteratea (a+1);
  in
    iteratea 1;
    Array.iteri (fun i v -> if (v > (!vmax)) then (imax := i; vmax := v)) p;
    (!imax)
;;

