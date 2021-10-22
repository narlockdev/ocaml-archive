(* You are welcome to read this file, but you do not need
   to understand (yet) its contents.  We will discuss 
   several aspects of this later in the semester. For now
   you only need to use it, not understand it.
 *)

open Lab_02

let eqf (f1: float) (f2: float) : bool = 
  (* We will not check floating point values for exact
     equality since rounding errors can derail this method.

     Instead we determine that the values are "close enough"
     together by checking that the absolute value of their 
     difference is within a specified value, `epsilon`.
   *)
  let epsilon = 0.01
  in
  if f1 > f2 then
    ((f1 -. f2) < epsilon) 
  else
    ((f2 -. f1) < epsilon) 

type test = unit -> unit

let check_test (f: unit -> bool) (f_str: string) : test =
  fun () ->
  if f () then
    print_endline ("PASSED: " ^ f_str)
  else
    print_endline ("FAILED: " ^ f_str)


let some_tests =
  [ check_test (fun () -> eqf 1.000001 1.000002)
                         "eqf 1.000001 1.000002";
    check_test (fun () -> eqf (circle_circum_v1 2.5) 15.70)
                         "eqf (circle_circum_v1 2.5) 15.70)";
    check_test (fun () -> eqf (circle_circum_v2 2.5) 15.70)
                         "eqf (circle_circum_v2 2.5) 15.70)";
    check_test (fun () -> eqf (power 2 3.0) 9.)
                         "eqf (power 2 3.0) 9.";
    check_test (fun () -> eqf (power 3 3.0) 27.)
                         "eqf (power 3 3.0) 27.";
    check_test (fun () -> eqf (power 3 3.2) 32.768)
                         "eqf (power 3 3.2) 32.768)";
    check_test (fun () -> eqf (cube 3.0) 27.0) 
                         "eqf (cube 3.0) 27.0";
    check_test (fun () -> eqf (cube 3.2) 32.768) 
                         "eqf (cube 3.0) 32.768";
  ]


let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests


