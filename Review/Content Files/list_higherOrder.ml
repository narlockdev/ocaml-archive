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

(* Example 1 
Write a function that adds up all the numbers in an integer list *)

(* Using List.fold_left *)
let suml (lst: int list) : int =
  List.fold_left ( + ) 0 xs

(* In this example, there is not an accumulator value
We should also note the `0` which represents more or
less the base value of this expression
-> what our first element is going to be applied to *)

(* In this example, we can go through the steps of Fold_left as follows: 

Consider suml [1;2;3]

= fold_left ( + ) 0 [1;2;3]
= fold_left ( + ) (0 + 1) [2;3]
= fold_left ( + ) ((0+1) + 2) [3]
= fold_left ( + ) (((0+1) + 2)) + 3) []
= (((0 + 1) + 2) + 3)
= 6 
*)

(* Let's write the same function, but with fold_right *)
let sumr (lst: int list) : int =
  List.fold_right ( + ) xs 0

(* We see now that our base value is going to be
represented as the second argument in our function 
(reminder that the function is simply ( + ) - the int add operation 
*)

(* Consider sumr [1;2;3] 

= fold_right (+) [1;2;3] 0 
= (+) 1 (fold_right (+) [2;3] 0)
= 1 + (fold_right (+) [2;3] 0)
= 1 + ( (+) 2 (fold_right (+) [3] 0)
= 1 + (2 + (fold_right (+) [3] 0))
= 1 + (2 + ( (+) 3 (fold_right (+) [] 0 ))
= 1 + (2 + (3 + (fold_right (+) [] 0)))
= 1 + (2 + (3 + 0))
= 6
*)

(* You can essentially write the fold expressions however you see
fit: if you think a fold right would work better than a fold left
in implementation: go for it; I personally like Fold_left as you
are always beginning with your base value *)

(* Fold Example 2 - using accumulators *)

(* Write a function (without recursion) that takes in
any list and returns the length of the list *)
let length (lst: 'a list) : int = 
  List.fold_left (fun length_sofar x -> length_sofar + 1) 0 lst

(* length_sofar represents this sort of accumulator value
Basically, our length_sofar will begin at 0 (since the length of
a list we don't know will begin at zero - the lowest possible
length we can have in a list is no elements whatsoever) 

Everytime a new element is read with the function, length_sofar is
incremented. Essentially the value of the previous call is still being
used. Hence this is why we call this an accumulator
*)

(* Fold Example 3 - using multiple accumulators *)

(* Write a function that takes in a list of integers. This function
will return the sum of the even positions in the list *)
let sum_even_positions (lst: int list) : int =
  let f (sum, position) (element) =
    if position mod 2 = 0
      then (sum + element, position + 1)
  else (sum, position + 1)
  in
  match List.fold_left f (0,0) lst with
  | (sum, pos) -> sum

(* 
Recall that even positions: 0, 2, 4
So If we call sum_even_postions (10::2::30::4::[])
We are doing 0 + 10 + 30 (the 0 representing initial accumualtor)

Notice that the accumulator in this example contains a tuple:
essentially stating that we are storing two different accumulators
inside of one. This tuple will be sent every time a fold occurs. 

The match at the end is sigificant: since if we didn't match, we
would always be returning a tuple with both accumulators. Since we 
only want the sum accumulator, we can match the tuple, and return
the sum (which is the int that will be returned) - we will receive
an error if we don't do it this way due to the function specifically
returning an integer by the definition
*)

(* Exercise 1 *)

(* Think: Which higher-order function should I use? *)

(* productf: function multiplies all the numbers in a float list
function satisfies product (l1 @ l2) = productf l1 *. product l2 *)
let productf (xs: float list) : float =
  List.fold_left ( *. ) 1. xs

(* Exercise 2 *)

(* minimum: function finds the smallest element in the list *)
let minimum (lst: 'a list) : 'a =
  let chooseMin min_sofar a' = if a' > min_sofar then min_sofar else a'
  in
  match lst with
  | [] -> raise (Invalid_argument "minimum")
  | x::rest -> List.fold_left chooseMin x rest

(* Exercise 3 *)

(* all_squares: function will return all the integers that are squares *)
let is_square x : bool =
  let sq = sqrt x in
  x = (sq *. sq)

let all_squares (xs: int list) : int list =
  List.filter (fun x -> is_square (float_of_int x)) xs

(* Exercise 4 *)

(* suffix: function uses string concaenation operator ^ to add
the string "!" to the end of each string in the input list *)
let suffix (s: string) (xs: string list) : string list =
  List.map (fun x -> x ^ s) xs

