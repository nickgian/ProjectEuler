let digitize number =
  let rec aux num acc =
    match num with 
      | n when n<10 -> (n::acc)
      | n -> aux (n/10) ((n mod 10)::acc)
  in 
    aux number []
;;


let palindrome lst =
  (lst = List.rev lst)
;;


let base2 number =
  let rec aux num acc =
    match num with
      | 0 -> acc
      | num -> aux (num / 2) ((num mod 2)::acc)
  in
    aux number []
;;

let solve =
  let sum = ref 0 in
    for i = 1 to 999999 do
      let num = digitize i in
        if (palindrome num) then
          (
            let binary = base2 i in
              if (palindrome binary) then
                sum:=(!sum)+i;
          )
    done;
    (!sum)
;;

