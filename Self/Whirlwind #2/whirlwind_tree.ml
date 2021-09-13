(* A binary tree data type *)
type tree = Empty
          | Fork of int * tree * tree

(* Sample trees *)
let t1 = Fork (3,
               Fork (2, Empty, Empty),
               Fork (4, 
                     Empty,
                     Fork (5, Empty, Empty)
                 )
           )

let t2 = Fork (7, t1, Empty)


let rec sum (t: tree) : int =
  match t with
  | Empty -> 0
  | Fork (n, left, right) -> n + sum left + sum right

let rec elem (i: int) (t: tree) : bool =
  match t with
  | Empty -> false
  | Fork (n, left, right) -> 
     if i = n
     then true
     else if i < n 
          then elem i left
          else elem i right

let rec insert (i: int) (t: tree) : tree = 
  match t with
  | Empty -> Fork (i, Empty, Empty)
  | Fork (n, left, right) ->
     if i = n then t
     else if i < n then Fork (n, insert i left, right)
                   else Fork (n, left, insert i right)

let t3 = insert 7 t1 