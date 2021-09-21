(* Hwk_01
Author: Anthony Narlock
Date: 9/21/2021
*)

(* sum: function adds up all the numbers in an integer list *)
let rec sum (xs: int list) : int =
  match xs with
  | [] -> 0
  | x::[] -> x
  | x::rest -> x + sum rest

(* sumf: function adds up all the numbers in a list of floating point numbers *)
let rec sumf (xs: float list) : float =
  match xs with
  | [] -> 0.
  | x::[] -> x
  | x::rest -> x +. sumf rest

(* product: function multiplies all the numbers in an integer list *)
(* function satisfies product (l1 @ l2) = product l1 *. product l2*)
let rec product (xs: int list) : int =
  match xs with
  | [] -> 0
  | x::[] -> x
  | x::rest -> x * product rest

(* productf: function multiplies a list of floating point numbers *)
(* function satisfies productf (l1 @ l2) = productf l1 *. productf l2*)
let rec productf (xs: float list) : float =
  match xs with
  | [] -> 0.
  | x::[] -> x
  | x::rest -> x *. productf rest

(* minimum: function finds the smallest element in the list.
It will likely use one of the operators < or > in doing this. (they work for different value types) *)
let rec minimum (xs: 'a list) : 'a =
  match xs with
  | [] -> raise (Invalid_argument "head")
  | x::[] -> x
  | x::rest -> if x < minimum rest then x else minimum rest

(* longest: function returns one of the longest strings in a list of strings.
USe the function String.length to determine the length of a string.
If two strings have the same length and are longer than any other strings, it is OK
to return either of them*)

let rec longest (xs: string list) : string =
  match xs with
  | [] -> raise (Invalid_argument "head")
  | x::[] -> x
  | x::rest -> if (String.length x) > (String.length (longest rest)) then x else longest rest


(* append: function appends two lists into one. 
One must decide which of the two lists to use in the match.
That is, one must decide which of the two input lists the function does recursion over *)
let rec append (xs: 'a list) (ys: 'a list): 'a list =
  match xs with 
  | [] -> ys
  | x::[] -> x :: ys
  | x::rest -> x :: (append rest ys)

(* elem: function will use the = operator to determine if a value is in the list
  Use || instead of if-then-else to do this. *)

(*COMPLETE THIS!!!*)
let rec elem (a: 'a) (xs: 'a list) : bool =
  match xs with
  | [] -> false
  | x::[] -> a = x
  | x::rest -> a = x || a = (elem a rest)