(* Higher-order Functions on Lists
author: Anthony Narlock
*)

(* What are higher-order functions?
Higher order functions are simply functions that take
other functions as arguments and return their functions as
their own results *)

(* Simple List higher order functions in OCaml include:

List.map : ('a -> 'b -> 'b) -> 'a list -> 'b = <fun> 
*** Map a function to a list ****
List.filter : ('a -> bool) -> 'a list -> 'a list = <fun> 
*** Returns the elements that satisfy bool expr ***

List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
List.fold_right : ('a -> 'b -> 'a) -> 'a list -> 'b -> 'b = <fun>

*** Folds essentially analyze a recursive structure through combination,
and recombine the results of recursively processing the parts to build
a value and return it ***

*** Folds typically use an `accumulator` value to store
a "list_sofar" or "something_sofar" to keep track of
the folds as they accumulate ***
*)

(* *** List.map examples *** *)

(* Map ex 1:
Write a function that uses the string concatenation
operator ^ to add the string ! to the end of each string
in the input list (no recursion) 
*)
let excited (lst: string list) : string list =
  List.map (fun x -> x ^ "!") lst

(* Also could be defined with helper function *)
let concat_exclaim (s: string) : string =
  s ^ "!"

let excited' (lst: string list) : string list =
  List.map concat_exclaim lst

(* Map ex 2:
Write a function that capitalizes each string in
a list. (Use String.capitalize_ascii)
*)
let capitalize_list (lst: string list) : string list =
  List.map (fun x -> String.capitalize_ascii x) lst

(* With helper function *)
let capitalize_string (s: string) : string =
  String.capitalize_ascii s

let capitalize_list' (lst: string list) : string list =
  List.map capitalize_string lst

(* Map example 3 
Write a function that squares every element in a list
*)
let square_list (lst: int list) : int list =
  List.map (fun x -> x * x) lst

(* *** List.filter Examples *** *)

(* Filter example 1 
Write a function that returns only the odd integers
in a list of integers *)
let only_odds (lst: int list) : int list =
  List.filter (fun x -> (x mod 2 != 0)) lst

(* Filter example 2
Write a function that only returns the capitalized
strings in a string list (use String.get to extract 
the first character from a string, check uppercase with
Char.uppercase_ascii) 
*)
let is_capital (s: string) : bool =
  if
    String.get s 0 = Char.uppercase_ascii (String.get s 0)
  then true
else false

let only_capitalized (lst: string list) : string list =
  List.filter is_capital lst

(* *** Fold Examples *** *)

(* There is a difference between folding left and
folding right *)