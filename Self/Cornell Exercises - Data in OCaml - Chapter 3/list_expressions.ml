(* list expressions
  Author: Anthony Narlock
  Date: 9/18/2021
*)

(* Construct a list that has the integers 1 through 5 in it.
Use the square bracket notation for lists *)
let ints = [1;2;3;4;5]

(* Construct the same list, but do not use the square bracket notation.
Instead, use :: and [] *)
let ints2 = 1 :: 2 :: 3 :: 4 :: 5 :: []

(* Construct the same list again. This time, the following expression must 
appear in your answer: [2;3;4]. Use the @ operator, and do not use :: *)

(* @ is the list append operator *)

let ints3 = [1] @ [2;3;4] @ [5]