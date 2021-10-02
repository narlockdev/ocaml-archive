(* OCaml Basics
author: Anthony Narlock 
*)

(* Hello World *)
print_string "Hello world" ;;

(* Expression Evaluation *)

(* Note, OCaml has no loops, it is a functional programming language.
To fix these, we use Recursion and Higher-order functions *)

(* Integer Operations *)
2 + 3 ;;
2 - 3 ;;
2 * 3 ;;
2 / 3 ;;

(* Floating point Operations - have a period after *)
2.3 +. 3.2 ;;
2.3 -. 3.2 ;;
2.3 *. 3.0 ;;
3. /. 2. ;;

(* Type System: strong, meaning OCaml can identify
the types of values - no declaration *)
let x = 2 ;;(* x is inferred to be an integer *)
let y = 2.0 ;;(* y is inferred to be a float *)

(* OCaml Type System 
int, float, bool, string, char 

Going back to the idea of integer/float operations, errors
yeild when we use the wrong operations on the different types

1 + "Hello" would yield an error since `+` is an integer
operation. "Hello" is a string, not an integer.

Other errors are caught such as divide by zero
*)

(* Name binding 
let-declaration and let-expressions *)

(* let-declaration *)
let var = 7 ;; (* the RHS is an expression that can be evaluated *)

(* let-expression *)
let var2 = 8 in var2 * var2 ;; (* RHS is an expr in expr - evalutes to 64 *)

(* Dynamic semantics for let expressions *)
let x = 1 + 4 in x * 3 ;; 

(* The Dynamic Semantics:
let x = e1 in e2; where e1 is 1 + 4 and e2 is x * 3 *)

(* Structured Data : Lists, Tuples, Records, Inductive Types *)
let lst = [1;2;3;4] ;;
let tup = (1,2,3,4) ;; 

let lst_of_tup = [(1,2) ; (3,4) ; (5, 6)] ;; 
let tup_of_list = ([1;2] , [3;4] , [5;6]) ;;

(* utop Top level 

Executing Code:
- Enter the directory using terminal
- type `utop` to open OCaml toplevel
- To use your file, type #use "<file-name>" ;; 
- To quit the toplevel, type #quit ;;
*)








