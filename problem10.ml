open Printf

let sieve (n : float) =
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
    let counter = ref 0;
    in   
    Array.iteri (fun indx value -> ( if ((indx!=0) && (indx != 1)  && (value == true)) then counter := !counter + indx; )) bitmap; Printf.printf "counter=%d \n" !counter ;;
