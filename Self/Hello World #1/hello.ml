(* Author: Anthony Narlock 

This is a simple hello world program written in OCaml

How to run this code:
- Obviously, have OCaml installed on the computer (specifically the compiler)
- Next, open Terminal and compile the code (if not compiled already)
This produces respective .byte (the bytecode), cmi, and cmo files
- ocamlc -o hello.byte hello.ml
- To run, type ./hello.byte

Alternatively, you can run this through building, this is typically
used for larger projects.
- ocamlbuild hello.byte
- _build/sanitize.sh
- rm hello.byte
- ocamlbuild hello.byte
- ./hello.byte
Clean up the compiled code
- ocamlbuild -clean

From here, just ocamlbuild hello.byte, then ./hello.byte

*)

let _ = print_endline "hello world"