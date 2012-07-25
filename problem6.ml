
let sum (n : float) = 1.0/.2.0 *. n *. (n +. 1.0)

let sum2 (n : float) = 1.0/.6.0 *. n *. (n +. 1.0) *. (2.0*.n +. 1.0)

let solve = (sum 100.0) *. (sum 100.0) -. (sum2 100.0);;
