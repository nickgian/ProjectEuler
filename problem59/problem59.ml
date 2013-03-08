let read_file () =
  let raw = read_line () in
  let cipher_text = List.map (int_of_string) (Str.split (Str.regexp_string ",") raw) in
  let split3 list = 
    let rec aux list xs ys zs = 
      match list with 
        | [] -> (List.rev xs, List.rev ys, List.rev zs) 
        | x :: [] -> aux [] (x :: xs) ys zs
        | x :: y :: [] -> aux [] (x :: xs) (y :: ys) zs
        | x :: y :: z :: list -> aux list (x :: xs) (y :: ys) (z :: zs)
    in
      aux list [] [] []
  in
    split3 cipher_text

let compute_lc xs =
  let lc = Hashtbl.create 52 in
  let rec aux xs = 
    match xs with
      | [] -> lc
      | x :: xs -> 
          (match (try Some (Hashtbl.find lc x) with Not_found -> None) with
             | None -> 
                 Hashtbl.add lc x 1; 
                 aux xs
             | Some count ->
                 Hashtbl.replace lc x (count + 1);
                aux xs)
  in
    aux xs
;;

let hashtbl_max hash = Hashtbl.fold (fun key v (k, max) -> if (max < v) then (key, v) else (k, max)) hash (0, 0)

let find_key xs ys zs =
  let lc1 = compute_lc xs in
  let lc2 = compute_lc ys in
  let lc3 = compute_lc zs in
  let (c1, _ ) = hashtbl_max lc1 in
  let (c2, _ ) = hashtbl_max lc2 in
  let (c3, _ ) = hashtbl_max lc3 in
  let (k1, k2, k3) = (c1 lxor 32 , c2 lxor 32, c3 lxor 32) in
    (k1, k2, k3)
;;

let decrypt xs key =
  List.map (fun x -> x lxor key) xs

let sum xs =
  List.fold_left (fun x acc -> x + acc) 0 xs

let zip3 x y z =
    let rec aux x y z acc =
        match (x, y, z) with
            | [], [], [] -> List.rev acc
            | x :: [], [], [] -> List.rev (x :: acc)
            | x :: [], y :: [], [] -> List.rev (y :: x :: acc) 
            | x :: xs, y :: ys, z :: zs -> aux xs ys zs (z :: y :: x :: acc)
    in
    aux x y z []
    
let rec print_plain lst =
    match lst with 
        | [] -> Printf.printf "\n"
        | x :: xs -> Printf.printf "%c" (char_of_int x); print_plain xs

let main =
    let (xs, ys, zs) = read_file () in
    let (k1, k2, k3) = find_key xs ys zs in
    let p_xs = decrypt xs k1 in
    let p_ys = decrypt ys k2 in
    let p_zs = decrypt zs k3 in
    let plaintext = zip3 p_xs p_ys p_zs in
     print_plain plaintext;
     Printf.printf "%d\n" ((sum p_xs) + (sum p_ys) + (sum p_zs))
;;
