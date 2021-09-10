(* The following functions came from Lecture on 9-10-2021 *)

(* A function to square numbers *)

let square (x: int) : int = x * x

(* My Note: This is a basic OCaml function.
A simple squaring function
- let keyword represents other language's def (Python) 
- (x: int) represents the parameter the function takes: name is x, type of x is an int
- : int x * x represents the return type (int) and the actual function body of what it returns (x * x)
*)

(* Summing numbers from 1 up to `n` *)

let rec sumTo (n: int) : int =
    if n = 0
    then 0
    else n + sumTo (n-1)

(* My Note:
In this function, the rec keyword represents that the function is a recursive function.
We see on line 19 that there is another call to the function, and that the function will end when n = 0
*)

(* Fibonacci *)

let rec fib = x
    if x = 0 then 0 else
        if x < 3 then 1 else fib (x-1) + fib (x-2)