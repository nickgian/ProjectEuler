let mklist =
    let rec aux n acc =
        match n with
        | 1 -> 1::acc
        | n -> aux (n-1) (n::acc)
    in
    aux 1000 [];;
    
let rec countWords nums cnt hash =
    match nums with
    | [] -> cnt
    | (n::nums) -> match n with
                   | 1000             ->   countWords nums (cnt+11) hash
                   |  n when n < 20  -> let str = Hashtbl.find hash n
                                       in
                                       countWords nums (cnt + (String.length str)) hash
                   |  n when n < 100 -> let units = (n mod 10) in
                                        let str1 = Hashtbl.find hash units in
                                        let str2= Hashtbl.find hash (n-units) in
                                          countWords nums (cnt + (String.length str1) + (String.length str2)) hash
                    
                   |  n              -> let units = (n mod 10)
                                         in
                                         let dec = ((n - units) mod 100)
                                         in
                                         let hundreds = (n/100)
                                         in
                                         let str1 = Hashtbl.find hash units in
                                         let str2 = Hashtbl.find hash dec in
                                         let str3 = Hashtbl.find hash hundreds in
                                         match (dec+units) with
                                         | 0 -> countWords nums (cnt + (String.length str3) + 7) hash
                                         | n when  n < 10 -> countWords nums (cnt + (String.length str1) + (String.length str3) + 10) hash
                                         | n  when n < 20 -> let str4 = Hashtbl.find hash n
                                                 in
                                                 countWords nums (cnt + (String.length str3) + (String.length str4) + 10) hash
                                         | n -> countWords nums (cnt + (String.length str1) + (String.length str2) + (String.length str3) + 10) hash                                       
;;


let solve =
    let hash = Hashtbl.create 20 in
        Hashtbl.add hash 0 "";
        Hashtbl.add hash 1 "one";
        Hashtbl.add hash 2 "two";
        Hashtbl.add hash 3 "three";
        Hashtbl.add hash 4 "four";
        Hashtbl.add hash 5 "five";
        Hashtbl.add hash 6 "six";
        Hashtbl.add hash 7 "seven";
        Hashtbl.add hash 8 "eight";
        Hashtbl.add hash 9 "nine";
        Hashtbl.add hash 10 "ten";
        Hashtbl.add hash 11 "eleven";
        Hashtbl.add hash 12 "twelve";
        Hashtbl.add hash 13 "thirteen";
        Hashtbl.add hash 14 "fourteen";
        Hashtbl.add hash 15 "fifteen";
        Hashtbl.add hash 16 "sixteen";
        Hashtbl.add hash 17 "seventeen";
        Hashtbl.add hash 18 "eighteen";
        Hashtbl.add hash 19 "nineteen";
        Hashtbl.add hash 20 "twenty";
        Hashtbl.add hash 30 "thirty";
        Hashtbl.add hash 40 "forty";
        Hashtbl.add hash 50 "fifty";
        Hashtbl.add hash 60 "sixty";
        Hashtbl.add hash 70 "seventy";
        Hashtbl.add hash 80 "eighty";
        Hashtbl.add hash 90 "ninety";
        countWords (mklist) 0 hash;;
        
