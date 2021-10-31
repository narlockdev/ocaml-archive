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

(* A binary tree type declaration *)

(* When we define a type, we will use type 'a do determine what the
value inside of each node of the tree will be; this could be anything
and thus why we use 'a *)

(* We know that each node of the tree can either be empty or not, if the 
node is not empty, we know that it has the possibility of having two
children, we will then determine if those children nodes are empty
or not empty.*)

(* Empty -> The node is empty
'a btree * 'a * 'a btree -> the node is not empty and thus we will
determine the left child (the first 'a btree), the value ('a) and 
the right child (the other 'a btree) 
Note: When we match a tree with something, when we call node
we will have to assign a left, value, and right parameter. *)

type 'a btree = Empty 
            | Node of 'a btree * 'a * 'a btree

(* An example of creating an integer btree: 
view image btree1.png for visual *)
let bt1 : int btree =
  Node (Node (Empty, 2, Empty),
        3,
        Node (Empty,
              4,
              Node (Empty, 5, Empty)
              )
        )

(* An example of creating a string btree:
Note: this is the same one as before, but with strings *)
let bt2 : string btree =
  Node (Node (Empty, "2", Empty),
        "3",
        Node (Empty,
              "4",
              Node (Empty, "5", Empty)
             )
       )

(* Let's now write some sample functions: 
We will use recursion and pattern matching
on our type that we have just created *)

(* A function that sums all the integers in a int btree *)
(* NOTE: If you sum nothing you will get zero *)
let rec sum (tree: int btree) : int =
  match tree with
  | Empty -> 0
  | Node (left, value, right) -> sum left + value + sum right

(* Exercise 1: Write a function that takes the product of all
the integers in a int binary tree *)

(* What property will we have to satisfy when we make a product function? 
Think: What wil happen if we call product on an empty binary tree? Or rather,
maybe an empty list? *)

let rec product (tree: int btree) : int =
  match tree with
  | Empty -> 1
  | Node (left, value, right) -> product left * value * product right

(* Let's write a function that converts a binary tree to a list *)
let rec btree_to_list (tree: 'a btree) : 'a list =
  match tree with
  | Empty -> []
  | Node (left, value, right) -> btree_to_list left @ (value :: []) @ btree_to_list right

(* Note that the expression (value :: []) could be simplified to [value], but for show
I will write it this way so it makes more sense that you can't append just a value 
Remember, it must be a list to append ! *)

(* I would like to point out that the runtime of btree_to_list is very slow,
it is O(N^2) since we are recalling and recalling the functions over and over again 
We want to somehow make this algorithm more efficient 

The problem with the runtime lies in the '@' operation. We are essentially re-checking
the value every time we are calling it, which can be horrible for runtime

We can do this similar to using an accumulator when we fold left or right, but
we won't be using a higher-order function to do this. *)

(* In this case, the accumulator will be "sofar" 
we will call a helper function that will track this sofar,
essentially it will be the actual function, except we will use
the heading of the function to only take in the parameter
of the tree we want, when essentially we could just call
helper tree [], we write our function to do that for us
so we don't have to send the empty list every time 

Note: sofar is beginning with the empty list, since
it is obvious that we are beginning with no list *)

let btree_to_list_accum (tree: 'a btree) : 'a list =
  let rec helper (t: 'a btree) (sofar: 'a list) =
    match t with
    | Empty -> sofar
    | Node (left, v, right) ->
      let right_result = helper right sofar in
      helper left (v :: right_result)
    in
    helper tree []

(* Exercise 2: Write a function that squares the elements in
an integer binary tree - return type is binary tree*)
let rec square_tree (tree: int btree) : int btree =
  match tree with
  | Empty -> Empty
  | Node (left, v, right) -> Node (square_tree left, v * v, square_tree right)

(* Exercise 3: Recall the List.map higher-order function, which essentially
took a function and applied that function to the values of the List

Now, write a function called tree_map that does exactly this: takes in a 
function for parameter, and applies the function across the values within the
binary tree *)
let rec tree_map (f: 'a -> 'b) (tree: 'a btree) : 'b btree =
  match tree with
  | Empty -> Empty
  | Node (left, value, right) -> Node (tree_map f left,
                                      f v,
                                      tree_map f right)

(* Exercise 4: Let's write a function to test our tree_map, let's use
Exercise 2 as an example. Re-write the square_tree function but now it 
uses tree_map to map the function into the values in the tree *)
let square_tree' t = tree_map (fun x -> x * x) t

(* Let's talk about folding over inductive types.
If the inductive type has 'n' different variants, then the
fold/reduce function takes n+1 arguments *)

(* Let's write a fold function for binary tree *)
let rec tree_fold (e: 'b) (n: 'b -> 'a -> 'b -> 'b) (t: 'a btree) : 'b =
  match t with
  | Leaf -> e
  | Node (left, v, right) -> n (tree_fold e n left) v (tree_fold e n right)

(* Exercise 5: Write a function that calculates the product of a tree *)
let btree_product (t: int btree) : int =
  tree_fold 1 (fun left v right -> left * v * right) t

(* Exercise 6: Write a function that calculates the height of a binary tree *)
let btree_height (t: 'a btree) : int =
  tree_fold 1 (fun left v right -> 1 + (max left right)) t 

(* Let's now consider a different kind of inductive type,
this type will be what's called a rose tree *)
type rose_tree = Rose of 'a * 'a rose_tree list

(* Now, let's write a fold function for this inductive type *)
let rt_fold (r: 'a -> 'b list -> 'b) (t: 'a rose_tree) : 'b =
  match t with
  | Rose (v, list) -> r v (List.map (rt_fold r) t)

(* Excerise 7: Write the same product function but on rose tree *)
let rt_product (t: int rose_tree) : int =
  let f (v: 'a) (vs: int list) : int =
    v * (fun xs -> List.fold_right ( * ) xs 1) vs
  in
  reduce f t

(* Think of as a function that multiplies the value by
the folded up list - the second component is a list remember! *)

(* Exercise 8: Write height for rose tree type *)
let height (t: 'a rose_tree) : int =
  let f (v: 'a) (vs: int list) : int =
    1 + (fun xs -> List.fold_right max xs 0) vs
  in
  reduce f t

