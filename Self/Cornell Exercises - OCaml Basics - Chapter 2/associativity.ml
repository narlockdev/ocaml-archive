(* associativity
  Author: Anthony Narlock
  Date: 9/18/2021
*)

(* Suppose we have the function add defined: *)
let add x y = x + y

(* What will the following code output? *)

add 5 1 ;;
- : int = 6

add 5 ;;
- : int -> int = <fun>

(add 5) 1 ;;
- : int = 6

add (5 1) ;;
Error: This expression has type int
This is not a function; it cannot be applied.