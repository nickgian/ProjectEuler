let maximum (a,b) (x,y) = if (a < x) then (x,y) else (a,b) ;;

let allPairs primes =
    let checkQuadratic a b =
        let rec aux n = if (primes.(abs(n*n + a*n + b))) then aux (n+1) else (n,a*b)
        in
         aux 0
    in
    let (prno,product) = (ref 0,ref 0) in
    for i = 1 to 1000 do
        for j = 1 to 1000 do
            if (primes.(i) && primes.(j) && primes.(i+j+1) ) then (
                let (no1,pr1) = checkQuadratic i j in
                let (no2,pr2) = checkQuadratic (-i) j in
                let (no3,pr3) = checkQuadratic i (-j) in
                let (no4,pr4) = checkQuadratic (-i) (-j) in
                let (max1,mpr1) = maximum (no1,pr1) (no2,pr2) in
                let (max2,mpr2) = maximum (no3,pr3) (no4,pr4) in
                let (maxx,pr) = maximum (max1,mpr1) (max2,mpr2) in
                    if ((maxx) > (!prno)) then (prno := maxx; product := pr;))
        done;
    done;
    ((!prno),(!product))
;;    

let sieve lim = 
    let bitmap = Array.make (lim+1) true 
    in   
    let root = truncate (sqrt (float lim))
    in   
    for i = 2 to root do
        if (bitmap.(i) == true) then 
            let j = ref (i*i) 
            in   
            while (!j <= lim) do
                bitmap.(!j) <- false;
                j := !j + i; 
            done;
    done;
    allPairs bitmap
;;

let solve = sieve 20000 ;;
