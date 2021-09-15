(* Write an OCaml function named power with type int -> float -> float *)
let rec power (p:int) (b:float) : float =
  if p = 0 then 1.0
  else b *. power (p-1) b

(* Using power, write a cube function *)
let cube = power 3

(* Essentially, whenever you call cube, it is writing power 3
Thus, typing
  cube 2
    is the same as
  power 3 2
*)

(* Writing a GCD function
Greatest Common Divisor
Ex: The GCD of integers 12 and 8 is 4

We want to begin by taking the smallest of the two inputs
then decrementing until we reach the divisor

But.. We can't "Decrement" in OCaml, meaning, we have to 
implement recursion somehow

We will do that by nesting a let rec function inside of our gcd function
*)

let gcd (x:int) (y:int) : int =
  let min = if x > y then x else y in
  let rec decrement m =
    if x mod m = 0 and y mod m = 0
      then m else decrement (m - 1)
  in
  decrement min 
