(* equality 
Author: Anthony Narlock
Date: 9/18/2021

structural equality: =
physical equality: ==

negation of structural equality: <>
negation of physical equality: !=
*)

(* Write an expression that compares 42 to 42 using structural equality *)
42 = 42 ;;
- : bool = true

(* Write an expression that compares "hi" to "hi" using structural equality *)
"hi" = "hi" ;;
- : bool = true

(* Write an expression that compares "hi" to "hi" using physical equality *)
"hi" == "hi" ;;
- : bool = false

(* Testing negation of physical equality *)
"hi" != "hi" ;;
- : bool = true

(* Testing negation of structural equality *)
"hi" <> "hi" ;;
- : bool = false