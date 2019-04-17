open OUnit2
open Diff

let approx_eq (f1:float) (f2:float) : bool = 
  if f1 -. f2 > 0.0001 || f2 -. f1 > 0.0001 then false 
  else true
  (* if (Float.compare (Float.abs (f1 - f2)) (0.0001)) = 1 
  then false else true *)

(** [ make_backward_test (name:string) (input:float) 
  (expected_output:float)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [approx_eq input expected_output. *) 
let make_backward_test (name:string) (input:float) 
(expected_output:float) =
name >::(fun _ -> assert_equal (approx_eq input expected_output) true)

(* Setting up test for x*x *)
let x2_var = init 42. 
let x2_output = StdOps.mul x2_var x2_var
let () = backward x2_output
let ans_x2 = get_grad x2_var 

(* Setting up test for x *)
let x_var = init 42. 
let x_output = x_var
let () = backward x_output
let ans_x = get_grad x_var 

(* Setting up test for x*x+4 *)
let eq1_var1 = init 42. 
let eq1_var2 = init 4. 
let eq1_output = StdOps.add (StdOps.mul eq1_var1 eq1_var1) eq1_var2 
let () = backward eq1_output
let ans_eq1 = get_grad eq1_var1

(* Setting up test for x*x+x+4 *)
let eq2_var1 = init 42. 
let eq2_var2 = init 4. 
let eq2_output = StdOps.add (StdOps.add 
(StdOps.mul eq2_var1 eq2_var1) eq2_var1) eq2_var2
let () = backward eq2_output
let ans_eq2 = get_grad eq2_var1

(* Setting up test for x^4 which is pow(x,4) *)
let eq3_var1 = init 42. 
let eq3_output = StdOps.pow eq3_var1 4.
let () = backward eq3_output
let ans_eq3 = get_grad eq3_var1

(* Setting up test for 3sin(3x)  *)
let eq4_var1 = init 42. 
let eq4_var2 = init 3. 
let eq4_output = StdOps.mul (StdOps.sin (StdOps.mul eq4_var1 eq4_var2)) eq4_var2
let () = backward eq4_output
let ans_eq4 = get_grad eq4_var1

let backward_tests = [
  make_backward_test "testing x*x" ans_x2 84.;
  make_backward_test "testing x" ans_x 1.;
  make_backward_test "testing x*x+4" ans_eq1 84.;
  make_backward_test "testing x*x+x+4" ans_eq2 85.;
  make_backward_test "testing x^4" ans_eq3 296352.;
  make_backward_test "testing  3 sin x" ans_eq4 8.49586;
]

let suite = 
 "test suite for A6"  >::: List.flatten [
    backward_tests;
]

let _ = run_test_tt_main suite