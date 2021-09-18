(* Cornell CS 3110 Exercises in OCaml
   Author: Anthony Narlock
     Date: September 17th, 2021
*)

(* 
   Exercise: Operators
   
   Write an expression that multiplies 42 by 10 

   # 42 * 10 ;;
   - : int = 420
     
  Write an expression that divides 3.14 by 2.0
   # 3.14 /. 2.0
   - : float = 1.57
     
*)

(* Write an expression that computes 4.2 rasied to the 7th power *)
let rec power (p:int) (b:float) : float =
  if p = 0 then 1.0
  else b *. power (p - 1) b 
         
         (* Typing power 7 4.2 will yield solution *)


