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

(* 
rec defines a recursive function
function name : fib
arguments : x
expr : RHS 

The type of fib is: 
val fib : int -> int = <fun>
*)

(* More examples of some simple functions in OCaml *)

(* #1 - Calculates circumference; only uses literals *)
let circle_circum_v1 (r:float) : float = 2.0 *. 3.1415 *. r

(* #2 - Calculates circumference; uses nested let for pi *)
let circle_circum_v2 (r:float) : float = 
  let pi = 3.1415 in
  2.0 *. pi *. r

(* #3 - Recursively Caclulates b^p, uses multiple arguments *)
let rec power (p:int) (b:float) : float =
  if p = 1 then b
  else b *. power (p-1) b

(* #4 - Calculates cube area uses recursive power function *)
let cube (x:float) : float =
  power 3 x

