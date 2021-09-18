(* infix average
  Author: Anthony Narlock
  Date: 9/18/2021
*)

(* Define an infix operator +/. to compute the average of two floating-point numbers *)
let (+/.) x y = (x +. y) /. 2.

10. +/. 5. ;;
- : float = 7.5