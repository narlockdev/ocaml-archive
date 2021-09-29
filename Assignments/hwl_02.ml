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

let minimum (lst: 'a list) : 'a =
  let chooseMin min_sofar a' = if a' > min_sofar then min_sofar else a'
  in
  match lst with
  | [] -> raise (Invalid_argument "minimum")
  | x::rest -> List.fold_left chooseMin x rest

(* TODO longest: function returns the longest strings in a string list *)
let longest (lst: string list) : string =
  let chooseLongString long_sofar s = 
    if (String.length long_sofar) > (String.length s) then long_sofar else s
  in
  match lst with
  | [] -> raise (Invalid_argument "longest")
  | x::rest -> List.fold_left chooseLongString x rest

(* append: function appends two lists into one *)
let append (xs: 'a list) (ys: 'a list) : 'a list =
  let addElement element list_sofar = element :: list_sofar
in
  List.fold_right addElement xs ys

(* elem: function will use the = operator to determine if a value is in the list
  Use || instead of if-then-else to do this. *)

let elem (element: 'a) (xs: 'a list) : bool =
  List.fold_left (fun boolState x -> boolState || (x = element)) false xs

(* excited: the function uses the string concatenation operator ^ to add
the string "!" to the end of each string in the input list *)
let excited (xs: string list) : string list =
  List.map (fun x -> x ^ "!") xs

(* suffix: function is a generalization on the one above. Adds a given string 
to ethe end of each string in a list *)
let suffix (s: string) (xs: string list) : string list =
  List.map (fun x -> x ^ s) xs

(* excited': function should have the same behavior as excited but should call
suffix to compute the resulting strings*)
let excited' (lst: string list) = suffix "!" lst

(* lengths: function returns the length of each string in the input
Might use String.length function in this one *)
let lengths (xs: string list) : int list =
  List.map (fun x -> String.length x) xs

(* capitalize: function will capitalize each string in a list. The function
String.capitalize_ascii might be useful here *)
let capitalize (xs: string list) : string list =
  List.map (fun x -> String.capitalize_ascii x) xs

(* all_odds: function may remind you of the one we did in class
returns all of the odd numbers in the input list *)
let all_odds (xs: int list) : int list =
  List.filter (fun x -> (x mod 2 != 0)) xs

(* all_capitalized: retuns all the strings that begin with a capitalized letter
Here yoiu might use the String.get function to extract the first character from
the string and then check if the character is the same as its capitalized version *)

let not_empty (s: string) : bool =
  if s = "" then raise (Invalid_argument "all_capitalized") else true
let equal_cap (s: string) : bool =
  if String.get s 0 = Char.uppercase_ascii (String.get s 0) then true else false

let all_capitalized (xs: string list) : string list =
  List.filter (fun x -> not_empty x && equal_cap x) xs

(* all_square: function will return all the integers that are squares *)
let is_square x : bool =
  let sq = sqrt x in
  x = (sq *. sq)

let all_square (xs: int list) : int list =
  List.filter (fun x -> is_square (float_of_int x)) xs

(* group: function takes a list of elements and groups them into pairs *)
let group (xs: 'a list) : ('a * 'a) list =
  let group_pair x y = (x , y)
in
match xs with
| [] -> []
| _::[] -> []
| x::x2 -> List.map (group_pair x x2) xs
  