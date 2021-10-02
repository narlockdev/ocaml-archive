(* Basic Functions 
author: Anthony Narlock
*)

(* Over simple data :

let <function-name> <arguments> = <expr> *)

let increment x = x + 1
(* 
function-name : increment
arguments : x
expression : x + 1

Notice that x is inferred to be type integer since
the operation in the expression can only be applied to
integer types.

val increment : int -> int = <fun>

calling something in utop like:
increment 2 ;;

will return:
- : int = 3
*)

let add x y = x + y
(*
function-name : add
arguments : x & y
expression : x + y

again, x and y are assumed to be integers for the same
reason as increment

val add : int -> int -> int = <fun>

calling
add 1 2 ;; 

will return
- : int = 3
*)

(* These functions can also be defined with the
intended argument types in mind. 

For example, I will rewrite add: *)

let add2 (x: int) (y: int) : int =
  x + y

(* This expression yields the same as add *)

(* Recursive Functions:

Recall that a recursive function is a function
that will call itself until a base case has been
satisfied.

"let rec" will define a recursive function in OCaml *)

(* fib function will return the nth number in the
fibonacci sequence : 1, 1, 2, 3, 5, 8, ...

This is an example of using recursion since it will
recall itself to receive numbers until the base case is found
*)

let rec fib x =
  if x = 0 then 0 else
    if x < 3 then 1 else fib (x - 1) + fib (x - 2)


(* Exercise 1: Write a function named circle_area that
takes in the radius and returns the area of the circle *)
let circle_area (r: float) : float =
  r *. r *. 3.14

(* Exercise 2: Write a function named power that takes in
a power `n` and a float `x` and returns x ^ n *)

(* 2^3 = 8 = 2 * 2 * 2 *)
let rec power (n: int) (x: float) : float =
  if n = 0 then 1.0 (* anything to the power of 0 is 1.0 *)
  else x *. power (n - 1) x

(* Exercise 3: Write a function named gcd that computes
the greatest common divisor of two positive integers *)

(* How do we attack this problem? 
- Pick a number that must be greater than or equal to the GCD
- Decrement it by one until it is a common divisor
*)

let gcd (x: int) (y: int) : int =
  let min = if x < y then x else y
  in
  let rec dec m =
    if x mod m = 0 && y mod m = 0
    then m 
    else dec (m-1)
  in
  dec min

