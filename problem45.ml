let pentNum n = (n * (3*n - 1)) lsr 1
;;

let isTriangle n =
    let fp =  sqrt(8.*.(float n) +. 1.) in
    fp = (floor fp)
;;

let isHexagonal n =
    let fp =  (sqrt(8.*.(float n) +. 1.) +.1. )/.4. in
    fp = (floor fp)
;;

let solve =
    let rec aux i =
        let num = pentNum i in
        match isTriangle num, isHexagonal num with
          | true,true -> num
          | _,_ -> aux (i+1)
    in
    aux 166
;;

