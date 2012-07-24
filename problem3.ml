open Printf

let sieve (m : float) =
    let n = sqrt(m)
    in   
    let lim = truncate (n)
    in   
    let bitmap = Array.make (lim+1) true 
    in   
    let root = truncate (sqrt n)
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
    Array.iteri (fun indx value -> ( if ((indx!=0) && (value == true) && ((600851475143 mod indx)==0)) then Printf.printf "i=%d value=%B \n" indx value)) bitmap ;;
