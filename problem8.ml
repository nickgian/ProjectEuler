window xs maxi =
        match xs with
            | a::b::c::d::e::xs -> let res = a*b*c*d*e
                                    in
                                    if (res > maxi) then window (b::c::d::e::xs) res else window (b::c::d::e::xs) maxi
            | _ -> maxi;;
            
    let explode str =
    let l = ref [] in
    let len = String.length str in
    for i = len - 1 downto 0 do
        l := (int_of_char str.[i] - 48) :: !l
    done;
    (!l)
    
    let solve str = window (explode str) 0;;
