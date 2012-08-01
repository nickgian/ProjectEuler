let y1901 = [Tuesday;Friday;Friday;Monday;Wednesday;Saturday;Monday;Thursday;Sunday;Tuesday;Friday;Sunday];;

type day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let nextDay day =
    match day with 
        | Monday -> Tuesday
        | Tuesday -> Wednesday
        | Wednesday -> Thursday
        | Thursday -> Friday
        | Friday -> Saturday
        | Saturday -> Sunday
        | Sunday -> Monday

let mapi f xs =
    let rec aux i xs acc =
    match xs with
    | [] -> List.rev acc
    | (x::xs) -> aux (i+1) xs ((f i x) :: acc)
    in
    aux 0 xs [];;

let shift (xs : day list) (year : int)=
    match (year mod 4) with
    | 2 | 3 -> List.map (fun x -> nextDay x) xs
    | 1 ->  mapi (fun i x -> if ((i == 0) || (i == 1)) then (nextDay (nextDay x)) else nextDay x) xs
    | 0 -> mapi (fun i x -> if ((i == 0) || (i == 1)) then nextDay x else nextDay (nextDay x)) xs
    | _ -> failwith "This value make sense. None" 

let rec solve xs year counter =
    match year with
    | 2001 -> counter
    | year -> solve (shift xs (year+1)) (year+1) (counter + (List.fold_left (fun sum x -> match x with
                                                                            | Sunday -> sum + 1
                                                                            | _ -> sum ) 0 xs));;
