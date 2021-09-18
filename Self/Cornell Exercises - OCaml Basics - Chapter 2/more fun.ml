(* more fun
Author: Anthony Narlock
Date: 9/18/2021
*)

(* Define a function that computes the code of a floating-point number *)
let cube x = x *. x *. x
val cube : float -> float = <fun>

cube 3. ;;
- : float = 27.

(* Define a function that computes the sign (1, 0, or -1) of an integer. *)

(* My solution using pattern matching *)
let sign (x:int) : int =
  match x with
  | 0 -> 0
  | x when (x > 0) -> 1
  | x when (x < 0) -> -1

sign 2 ;;
- : int = 1
sign 1 ;;
- : int = 1
sign (-1) ;;
- : int = -1
sign (-200) ;;
- : int = -1
sign 0 ;;
- : int = 0

(* Define a function that computes the area of a circle given its radius. *)
let circle_area r = 3.14 *. r *. r

circle_area 2. ;;
- : float = 12.56

assert ((circle_area 2.) = 12.56) ;;
- : unit = ()

assert ((circle_area 2.) = 12.) ;;
Exception: Assert_failure ("//toplevel//", 1, 0).

(* Define a function with multiple inputs x y and z that calculate the average of three arguments *)
let avg3 x y z =
  (x +. y +. z) /. 3