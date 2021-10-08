(* Inductive Types in OCaml
Author: Anthony Narlock
*)

(* Insert explanation *)

(* A type msg can either be a StringMsg, BoolMsg, or FloatMsg, each of
these are represented as a "constructor", where they take in their
respective types *)

type msg = StringMsg of string * int
         | BoolMsg of bool * int
         | FloatMsg of float * int

(* Shows an example of storing msg type in a list, showing
how to create multiple msg types and display how their
constructor is used to do so *)         

let sample_msgs = [
    StringMsg ("Hello", 234);
    StringMsg ("World", 235);
    BoolMsg (true, 245);
    BoolMsg (false, 248);
    FloatMsg (3.14, 260);
    StringMsg ("Bye!", 280)
  ]

  (* Example of sending a message through a function, it
  must do different things depending on what type of message
  it is. For example, consider msg_log, we want to make sure
  that the boolean messages and float messages are being properly
  handled (using their respective functions) *)

  let msg_log (m: msg) : string =
    match m with
    | StringMsg  (s, ts) -> 
       "Msg at " ^ string_of_int ts ^ ": " ^ s
    | BoolMsg (b, ts) -> 
       "Msg at " ^ string_of_int ts ^ ": " ^ string_of_bool b
    | FloatMsg (f, ts) -> 
       "Msg at " ^ string_of_int ts ^ ": " ^ string_of_float f

let log_messages (msgs: msg list) : string =
  String.concat "\n" (List.map msg_log msgs)
      
let print_messages (msgs: msg list) =
  print_endline (log_messages msgs)