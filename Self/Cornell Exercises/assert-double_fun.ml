(* assert & double fun
Author: Anthony Narlock
Date: 9/18/2021
*)

(* assert *)

(* Enter assert true ;; into utop *)
assert true ;;
- : unit = ()

(* Enter assert false ;; into utop *)
assert false ;;
Exception: Assert_failure ("//toplevel//", 1, 0).

(* Write an expression that asserts 2110 is not (structurally) equal to 3110 *)
assert (2110 = 3110) ;;
Exception: Assert_failure ("//toplevel//", 1, 0).

(* double fun *)

(* Using the increment function from above as a guide, define a function double 
that multiplies its input by 2. For example, double 7 would be 14. Test your function
by applying it to a few inputs. Turn those test cases into assertions *)

let double x = x * 2

double 7 ;;
- : int = 14

double 12 ;;
- : int = 24

assert ((double 7) = 14) ;;
- : unit = ()

assert ((double 7) = 15) ;;
Exception: Assert_failure ("//toplevel//", 1, 0).
