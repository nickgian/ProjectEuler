open Euler

let main =
  let sieve = Primes.sieve 1000 in
  let rec solve i product = 
      match sieve.(i) with
        | true -> solve (i+1) (product * i)
        | false when (product > 1000000) -> product / (i-1)
        | false -> solve (i+1) product
  in
    solve 1 1
