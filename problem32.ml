let checkPandigital a b c =
  let res = ['1';'2';'3';'4';'5';'6';'7';'8';'9'] in
  let str1 = string_of_int a in
  let str2 = string_of_int b in
  let str3 = string_of_int c in
  let str = String.concat "" [str1;str2;str3] in
  let theList = ref [] in
  String.iter (fun x -> theList := (x::(!theList))) str;
  let sorted = List.sort compare (!theList) in
  if (sorted = res) then true else false;;

let checkDivisors num =
  let lim = truncate (sqrt (float num)) in
  let rec aux i =
    match i with
    | i when i = lim -> 0
    | i -> if ((num mod i) = 0) then 
            (if (checkPandigital num i (num/i)) then num else aux (i+1))
           else aux (i+1)
  in
    aux 2;;

let cmp a b c d = ((a != b) && (a != c) && (a != d)
                    && (b != c) && (b != d) &&(c != d))
;;

let solve =
  let sum = ref 0 in
  for i = 1 to 9 do
    for j = 1 to 9 do
      for k = 1 to 9 do
        for l = 1 to 9 do
            if (cmp i j k l) then sum := (!sum) + checkDivisors (i*1000 + j*100 + k*10 + l);
        done;
      done;
    done;
  done;
  Printf.printf "%d\n" (!sum);;
