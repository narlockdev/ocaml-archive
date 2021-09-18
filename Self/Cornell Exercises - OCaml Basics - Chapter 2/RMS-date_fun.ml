(* RMS & date_fun
Author: Anthony Narlock
Date: 9/18/2021
*)



(* Define a function that computes the root-mean-square of two numbers 
sqrt((x^2 + y^2) / 2)  *)
let squareFloat x = x *. x

let rootMeanSquareFloat x y =
  sqrt( ((square x) +. (square y)) /. 2.)

(* Define a function that takes an integer d and string m as input and returns
true just when d and m form a valid date. Here, a valid date has a month that
is one of the following abbreiations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept,
Oct, Nov, Dec. And the day must be a number that is between 1 and the minimum number 
of days in that month, inclusive.

Ex: If the month is Jan, then the day is between 1 and 31, inclusive, whereas
If the month is Feb, then the day is between 1 and 28, inclusive *)

let days31 = ["Jan";"Mar";"May";"Jul";"Aug";"Oct";"Dec"] 
let days30 = ["Apr";"Jun";"Sept";"Nov"]
             
let date (d:int) (string:m) : bool =
  if (List.mem m days31) then
    if (d > 0 && d < 32) then true else false 
        
  else if (List.mem m days30) then
    if (d > 0 && d < 31) then true else false

  else if (m = "Feb") then
    if (d > 0 && d < 29) then true else false

  else false


