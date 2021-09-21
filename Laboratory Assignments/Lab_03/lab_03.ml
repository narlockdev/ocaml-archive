(* Lab_03 - Lists in OCaml
Author: Anthony Narlock
Date: 9/21/2021
*)

open Char

(* Sum of a list of floats 
Write a similar function named sum_of_floats that computes the sum of the values
in a list of floats. It will have type float list -> float 
*)

let rec sum_of_floats (xs: float list) : float =
  match xs with
  | [] -> 0.
  | x::rest -> x +. sum_of_floats rest

(* Square all the floats in a list 
Write a function square_floats of type float list -> float list that squares all of the
float values in a list.
*)

let rec square_floats (xs: float list) : float list =
  match xs with
  | [] -> []
  | x::[] -> (x *. x) :: []
  | x::x2::[] -> (x *. x) :: (x2 *. x2) :: []
  | x::x2::rest -> (x *. x) :: (x2 *. x2) :: square_floats rest

(* Append "!" to all of the strings in a list 
Write a function called exclaimify that appends a ! symbol to all strings in a list.
This function has type string list -> string list
*)

let rec exclaimify (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] -> (x ^ "!") :: []
  | x::x2::[] -> (x ^ "!") :: (x2 ^ "!") :: []
  | x::x2::rest -> (x ^ "!") :: (x2 ^ "!") :: exclaimify rest

(* Remove all odd numbers from a list 
Write a function remove_odds of type int list -> int list that removes all odd numbers
from a list of integers *)

let odd x = x mod 2 != 0

let rec remove_odds (xs: int list) : int list =
  match xs with
  | [] -> []
  | x::[] -> if odd x then [] else x :: []
  | x::rest -> if odd x then remove_odds rest else x :: remove_odds rest

(* Remove all strings beginning with a captial letter from a list 
Write a function remove_caps of type string list -> string list to remove all the 
strings beginning with a captial letter from a list of strings *)

let rec remove_caps (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] -> 
      let c = Char.uppercase (String.get x 0)
      in if (String.get x 0 = c) then [] else x :: []
  | x::rest ->
      let c = Char.uppercase (String.get x 0)
      in if (String.get x 0 = c) then remove_caps rest else x :: remove_caps rest


