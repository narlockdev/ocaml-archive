(* 
We can think of the `list` type as being defined as follows,
even though this isn't quite valid OCaml code and the list
type is built in.
But it has 2 constructors, the :: constructor being the interesting
inductive one.
type 'a list = []
             | :: of 'a * 'a list
*)

let rec sum xs =
  match xs with
  | [] -> 0
  | x::rest -> x + sum rest

let rec all (bs : bool list) : bool = 
  match bs with
  | [] -> true
  | b::rest -> b && all rest

let even x = x mod 2 = 0

let rec even2ways xs =
  match xs with
  | [] -> true
  | x::[] -> false
  | x1::x2::rest -> even x1 && even x2 && even2ways rest