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

let rec elem (element: 'a) (xs: 'a list) : bool =
  match xs with
  | [] -> false
  | x::rest -> x = element || elem element rest

(* excited: the function uses the string concatenation operator ^ to add
the string "!" to the end of each string in the input list *)
let rec excited (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] -> (x ^ "!") :: []
  | x::rest -> (x ^ "!") :: excited rest

(* suffix: function is a generalization on the one above. Adds a given string
to the end of each string in a list *)
let rec suffix (s: string) (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] -> (x ^ s) :: []
  | x::rest -> (x ^ s) :: suffix s rest

(* excited': function should have the same behavior as excited but should call
suffix to compute the resulting strings *)
let excited' (lst: string list) = suffix "!" lst

(* lengths: function returns the length of each string in the input 
Might use String.length function in this one *)
let rec lengths (xs: string list) : int list =
  match xs with
  | [] -> []
  | x::[] -> (String.length x) :: []
  | x::rest -> (String.length x) :: lengths rest

(* length_pairs: function will compute the length of the strings in the input
list, like the function above, but it returns the string and its length in a pair
Thus the result is a list of pairs, each pair containing a string and an int *)
let rec length_pairs (xs: string list) : (string * int) list =
  match xs with
  | [] -> []
  | x::[] -> (x , (String.length x )) :: []
  | x::rest -> (x , (String.length x)) :: length_pairs rest

(* capitalize: function will capitalize each string in a list. The function
String.capitalize_ascii might be usefull here *)
let rec capitalize (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] -> (String.capitalize_ascii x) :: []
  | x::rest -> (String.capitalize_ascii x) :: capitalize rest

(* all_odds: function may remind you of the one we did in class
returns all of the odd numbers in the input list *)
let rec all_odds (xs: int list) : int list =
  match xs with
  | [] -> []
  | x::[] -> if (x mod 2 = 0) then [] else x :: []
  | x::rest -> if (x mod 2 = 0) then all_odds rest else x :: all_odds rest

(* all_capitalized: 
returns all the strings that begin with a capitalized letter. Here you might use
the String.get function to extract the first character from the string and then
check if the character is the same as its capitalized version *)
let rec all_capitalized (xs: string list) : string list =
  match xs with
  | [] -> []
  | x::[] ->
    let c = Char.uppercase_ascii (String.get x 0)
    in if (String.get x 0 = c) then x :: [] else []
  | x::rest ->
    let c = Char.uppercase_ascii (String.get x 0)
    in if (String.get x 0 = c) 
      then x :: all_capitalized rest 
      else all_capitalized rest

(* all_squares: function will return all the integers that are squares *)
let is_square x : bool =
  let sq = sqrt x in
  x = (sq *. sq)

let rec all_squares (xs: int list) : int list =
  match xs with
  | [] -> []
  | x::[] -> if is_square (float_of_int x) = true then x :: [] else [] 
  | x::rest -> if is_square (float_of_int x) = true then x :: all_squares rest else all_squares rest
  
(* group: function take a list of elements and groups them into pairs *)
let rec group (xs: 'a list) : ('a * 'a) list =
  if List.length xs mod 2 != 0 then raise (Invalid_argument "odd list")
  else
  match xs with
  | [] -> []
  | _::[] -> []
  | x::x2::rest -> (x, x2) :: group rest
  
(* unzip: function takes a list of pairs and unzips them into a pair of lists 

for any list strs of type string list,
(strs, lengths strs) = unzip (length pairs strs) *)
let rec unzip (lst: ('a * 'b) list) : ('a list * 'b list) =
  match lst with
  | [] -> ([],[])
  | (x,y)::[] -> (x::[],y::[])
  | (x,y)::(x2,y2)::[] -> (x::x2::[],y::y2::[])
  | (x,y)::(x2,y2)::rest -> (x::x2::fst (unzip rest) , y::y2::snd (unzip))