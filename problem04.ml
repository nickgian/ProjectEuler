let rec isPal x i n =  
    if (i >= n -i - 1) then true 
    else
      if (x.[i] = x.[n-i-1]) then isPal x (i+1) n else false

let isPalindrome x =  
    match x with 
        | "" -> true 
        | x -> isPal x 0 (String.length x)

    

let rec mklist num acc =
    match num with
        | 999 -> 999::acc
        | _ -> mklist (num+1) (num::acc)


let rec mult lst acc =
    match lst with
        | [] -> flatten acc
        | (h::t) -> mult t ((fold_left (fun lst x -> (h,x)::lst) [] (h::t))::acc) ;;

let solve = fold_left ( fun a (x,y) -> 
    let res = x*y in
        if (isPalindrome(string_of_int(res))) then 
        if (res > a) then res else a
        else a ) 0 (mult (mklist 100 []) []);;                            

