    let solve myarr = 
        let cnt = ref 0 in
        Array.iteri (fun ind x -> 
                begin 
                    if ((ind+3 < 400) && (((ind+3) mod 20)>2)) then 
                         begin 
                           let right = x*myarr.(ind+1)*myarr.(ind+2)*myarr.(ind+3) in
                           if (right>(!cnt)) then begin cnt:=right; Printf.printf "%d right\n" ind; end
                         end;
                    if (ind+60 < 400) then 
                         begin 
                         let down =  x*myarr.(ind+20)*myarr.(ind+40)*myarr.(ind+60) in
                           if (down>(!cnt)) then begin cnt:=down; Printf.printf "%d down\n" ind; end
                         end;
                    if ((ind-60+3 >= 0) && (((ind+3-60) mod 20) >2)) then 
                        begin 
                        let diagonup = x*myarr.(ind-20+1)*myarr.(ind-40+2)*myarr.(ind-60+3) in
                           if (diagonup>(!cnt)) then begin cnt:=diagonup; Printf.printf "%d digoonup\n" ind; end
                         end;
                    if ((ind+60+3 < 400) && (((ind+3+60) mod 20) > 2)) then 
                        begin 
                        let diagondown = x*myarr.(ind+20+1)*myarr.(ind+40+2)*myarr.(ind+60+3) in
                           if (diagondown>(!cnt)) then begin cnt:=diagondown; Printf.printf "%d diagondown\n" ind; end
                         end;
                end
        ) myarr;
        Printf.printf "%d \n" !cnt;; 
        
        let main =
             let in_channel = open_in Sys.argv.(1) in
             let read_int () = Scanf.fscanf in_channel "%d " (fun n->n) in 
             let myArray = Array.make (400) 0 in
                for i=0 to 399 
                do
                    myArray.(i)<-(read_int())
                done;
                solve myArray
        ;;
