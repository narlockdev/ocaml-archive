(* fib
  Author: Anthony Narlock
  Date: 9/18/2021
*)

(* Define a recursive function fib : int -> int, such that fib n is the nth number in the
Fibonacci sequence, which is 1, 1, 2, 3, 5, 8, 13 *)

let rec fib (n:int) : int =
  if (n = 1 || n = 2) then 1
  else fib (n-1) + fib (n-2)