(* Lab_02
Author: Anthony Narlock
Date: September 14th, 2021
*)

(* #1 - Calculates circumference; only uses literals *)
let circle_circum_v1 (r:float) : float = 2.0 *. 3.1415 *. r

(* #2 - Calculates circumference; uses nested let for pi *)
let circle_circum_v2 (r:float) : float = 
  let pi = 3.1415 in
  2.0 *. pi *. r

(* #3 - Recursively Caclulates b^p, uses multiple arguments *)
let rec power (p:int) (b:float) : float =
  if p = 1 then b
  else b *. power (p-1) b

(* #4 - Calculates cube area uses recursive power function *)
let cube (x:float) : float =
  power 3 x

(* This marks the end of the file *)
