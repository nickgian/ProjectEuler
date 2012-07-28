open Big_int
open Printf

let rec sum xs acc =
    match xs with
    | [] -> acc
    | (x::xs) -> sum xs (Big_int.add_big_int acc x)
        

let rec read_file file acc =
    match try Some (input_line file) with End_of_file -> None with 
    | Some line -> read_file file ((Big_int.big_int_of_string line)::acc)
    | None -> acc

let read filename = read_file (open_in filename) []

let solve filename = let big = Big_int.string_of_big_int (sum (read filename) Big_int.zero_big_int)
                     in
                     Printf.printf "%s\n" (String.sub big 0 10);;
