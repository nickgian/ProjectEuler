let rec withOne num coin restcoins acc =
        if (coin=1) then 1
        else
          match num with 
            | i when i < coin -> acc
            | i -> withOne (num-coin) coin restcoins (acc+(ways (num-coin) restcoins))

and ways num coins =      
    let rec aux num coins acc = 
        match coins with 
         |   [] -> acc
         |   head::tail ->  aux num tail (acc + (withOne num head tail 0))
    in
    aux num coins 0
;;

ways 200 [200;100;50;20;10;5;2;1];;
