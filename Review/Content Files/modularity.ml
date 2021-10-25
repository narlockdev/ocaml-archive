(* modularity - OCaml 
Author: Anthony Narlock
*)

(* Consider if I two different types, a list and a tree, and I defined
  a function with the same name on both of them, if I compiled and ran 
  code, OCaml would only take the most recent definition of that function. 

To fix this, we will implement a module. *)

module MyList = struct
  type 'a myList =
  | Empty -> Empty
  | Cons of 'a * 'a myList

  let rec map f = function
  | Empty -> Empty 
  | Cons (value, rest) -> Cons (f value, map f rest)
end

module Tree = struct
  type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

  let rec map f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map f l, map f r)
end

(* Now, the word map is unbound because it is not defined at
the top level of the scope 

In order to call examples on map now, we must call <Module-name>.map *)
let lst = MyList.map succ (Cons (1, Empty))

let t = Tree.Node (1, Leaf, Leaf)

