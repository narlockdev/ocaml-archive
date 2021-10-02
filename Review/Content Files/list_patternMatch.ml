(* Pattern Matching with Lists
author: Anthony Narlock
*)

(* List Operations:

Building a list:
[] is the empty list
:: "cons" operator - assigns two elements together
    2::[] will return a int list = [3]
    3::[2;3;4] will return an int list = [3,2,3,4]
[e1;e2;e3] - directly makes a list
*)

(*
Intro to pattern matching:
*)

(* Ex1: Write an OCaml function that returns true if 
  all elements of the list are true *)
let rec all (lst: bool list) : bool =
  match lst with
  | [] -> true
  | x::rest -> x && all rest

(* Ex2: Write a function even2ways that checks if an integer
list only contains even values and has an even number
of elements
*)
let even x = x mod 2 = 0

let rec even2ways (lst: int list) : bool = 
  match lst with
  | [] -> true
  | x::[] -> false (*only one element*)
  | x1::x2::rest -> even x1 && even x2 && even2ways rest

(* Example using _ 
_ essentially means that we don't care what the value
is inside of the list

We use this when we match with element, but we don't
do anything with the knowledge of the value that
is inside of that element *)

(*Ex3: is_empty: Write a function that returns true if
  the list is empty, and false otherwise *)
let is_empty (lst: 'a list) : bool =
  match lst with
  | [] -> true
  | _::_ -> false

(* Ex4: Write an OCaml function named head that returns
the front element of the list *)
let head (lst: 'a list) : 'a =
  match lst with
  | x::_ -> x
  | _ -> raise (Invalid_argument "head")

(* Exercise1 : Write a function that adds up all the numbers
in an integer list *)
let rec sum (xs: int list) : int =
  match xs with
  | [] -> 0
  | x::[] -> x
  | x::rest -> x + sum rest

(* Exercise2: Write a function that finds the smallest element
in the list. *)
let rec minimum (xs: 'a list) : 'a =
  match xs with
  | [] -> raise (Invalid_argument "minimum")
  | x::[] -> x
  | x::rest -> if x < minimum rest then x else minimum rest

(* Exercise3: Write a function that appends two lists into one *)
let rec append (xs: 'a list) (ys: 'a list): 'a list =
  match xs with 
  | [] -> ys
  | x::[] -> x :: ys
  | x::rest -> x :: (append rest ys)

(* Exercise4: Write a function that determines if a value is
in a list or not *)
let rec elem (element: 'a) (xs: 'a list) : bool =
  match xs with
  | [] -> false
  | x::rest -> x = element || elem element rest

(* Exercise5: Write a function that uses the string 
concatenation operator ^ to add the string ! to the end
of each string in the input list *)
let rec excited (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] -> (x ^ "!") :: []
  | x::rest -> (x ^ "!") :: excited rest

(* Exercise6: Write a function that will compute the length
of a string given in the input list. Return a tuple (pair) list
with each pair containing the (string, length of string) *)
let rec length_pairs (xs: string list) : (string * int) list =
  match xs with
  | [] -> []
  | x::[] -> (x , (String.length x )) :: []
  | x::rest -> (x , (String.length x)) :: length_pairs rest

(* Exercise 7: Write a function that takes a list of pairs
and unzips them into a pair of lists *)
let rec unzip (lst: ('a * 'b) list) : ('a list * 'b list) =
  match lst with
  | [] -> ([],[])
  | (x,y)::[] -> (x::[],y::[])
  | (x,y)::(x2,y2)::[] -> (x::x2::[],y::y2::[])
  | (x,y)::(x2,y2)::rest -> (x::x2::fst (unzip rest) , y::y2::snd (unzip rest))

