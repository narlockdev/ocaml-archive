(* Pattern Matching
Author: Anthony Narlock
Date: 9 / 17/ 2021 *)

(* Write an OCaml function named all that returns
true if all elements of the list are true *)

let rec all (lst: bool list) : bool = 
  match lst with
  | [] -> true
  | x::rest -> x && all rest

(* Write a function even2ways that checks if an integer
  list only contains even values and has an even
  number of elements *)

  let even x = x mod 2 = 0

  let rec even2ways (lst : int list) : bool =
    match lst with
    | [] -> true
    | x::[] -> false (* only one element *)
    | x1::x2::rest -> even x1 && even x2 && even2ways rest

(* Write an OCaml function named is_empty that returns
true if the list is empty, and false otherwise *)

  let is_empty (xs: 'a list) : bool =
    match xs with
    | [] -> true
    | _::_  -> false

(* Write an OCaml function named head that returns
the front element of the list *)
  
  let head (xs: 'a list) : 'a = 
    match xs with
    | x::_ -> x
    | _ -> raise (Invalid_argument "head")