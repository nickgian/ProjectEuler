open Euler

let perfectSquare n =
    let fp = sqrt n in
    fp = (floor fp)
;;

let solve =
    let primes = Primes.sieve 10000 in
    primes.(1) <- true;
    let checkGoldbach n =
        let rec iterPrimes i =
            match primes.(i) with
              | true -> (
                            match i with
                              | pr when (pr>n) -> false
                              | pr -> if perfectSquare ((float (n - pr))/.2.) then true
                                      else iterPrimes (i+1))
              | false -> iterPrimes (i+1)
        in
        iterPrimes 1
    in
    let rec iterArray i =
        match primes.(i), (isOdd i)  with
          | false, true -> if (checkGoldbach i) then iterArray (i+1) else i
          | _,_ -> iterArray (i+1)
    in
    iterArray 33
;;
