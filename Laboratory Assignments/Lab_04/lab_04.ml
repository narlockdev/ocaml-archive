(* Lab_04 - Higher Order Functions
Author: Anthony Narlock
Date: 9/27/2021
*)

open Char

(* Sum of a list of floats - fold *)
let sum_of_floats (lst: float list) : float =
  List.fold_left (+.) 0. lst

(* Square all the floats in a list - map *)
let square_floats (lst: float list) : float list =
  let sq (x: float) : float = x *. x
  in
  List.map sq lst
  
(* Append ! to all of the strings in a list - map *)
let exclaimify (l: string list) : string list =
  let ex x = x ^ "!"
  in
  List.map ex l

(* Remove all odd numbers from a list - filter *)
let remove_odds (xs: int list) : int list =
  let even x = x mod 2 = 0
  in
  List.filter even xs

(* Remove all strings beginning with a capital letter from a list *)

(* Helper function to check if the character is in the alphabet *)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let check_cap x = (String.get x 0 = (Char.uppercase_ascii (String.get x 0))
                   && is_alpha (Char.uppercase_ascii (String.get x 0))) = false

let remove_caps (lst: string list) : string list =
  List.filter check_cap lst
