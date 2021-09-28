(* Hwk_02
Author: Anthony Narlock
Date: 9/27/2021

Asked to implement all the functions from hwk 01 again. But this time,
asked to use one of List.map, List,filter, List.fold_left, or List.fold_right
in the implementation
*)

(* sum: function adds up all the numbers in an integer list *)
let sum (xs : int list) : int =
  List.fold_left (+) 0 xs

(* sumf: function adds up all the numbers in a list of floating point numbers *)
let sumf (xs : float list) : float =
  List.fold_left (+.) 0. xs

(* product: function multiplies all the numbers in an integer list 
function satisifes product (l1 @ l) = product l1 * product l2 *)
let product (xs: int list) : int =
  List.fold_left ( * ) 1 xs

(* productf: function multiplies all the numbers in a float list
function satisfies product (l1 @ l2) = productf l1 *. product l2 *)
let productf (xs: float list) : float =
  List.fold_left ( *. ) 1. xs

(* minimum: function finds the smallest element in the list *)

(*helper:*)
let min 

let minimum (xs: 'a list) : 'a =
  List.filter min xs