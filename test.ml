open OUnit2
open Diff

let approx_eq (f1:float) (f2:float) : bool = 
  if f1 -. f2 > 0.0001 || f2 -. f1 > 0.0001 then false 
  else true
  (* if (Float.compare (Float.abs (f1 - f2)) (0.0001)) = 1 
  then false else true *)

(* (** [ make_backward_test (name:string) (input:float) 
  (expected_output:float)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [approx_eq input expected_output]. *) 
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
let eq4_output = StdOps.mul (StdOps.sin 
(StdOps.mul eq4_var1 eq4_var2)) eq4_var2
let () = backward eq4_output
let ans_eq4 = get_grad eq4_var1

(* Setting up test for x^8  *)
let eq5_var1 = init 42. 
let eq5_output = StdOps.pow eq5_var1 8.
let () = backward eq5_output
let ans_eq5 = get_grad eq5_var1

(* Setting up test for x^15  *)
let eq6_var1 = init 42. 
let eq6_output = StdOps.pow eq6_var1 15.
let () = backward eq6_output
let ans_eq6 = get_grad eq6_var1

(* Setting up test for sin x  *)
let eq7_var1 = init 42. 
let eq7_output = StdOps.sin eq7_var1
let () = backward eq7_output
let ans_eq7 = get_grad eq7_var1

(* Setting up test for sin 3x  *)
let eq8_var1 = init 42. 
let eq8_var2 = init 3. 
let eq8_output = StdOps.sin (StdOps.mul eq8_var1 eq8_var2)
let () = backward eq8_output
let ans_eq8 = get_grad eq8_var1

(* Setting up test for cos x  *)
let eq9_var1 = init 42. 
let eq9_output = StdOps.cos eq9_var1
let () = backward eq9_output
let ans_eq9 = get_grad eq9_var1

(* Setting up test for cos 3x  *)
let eq10_var1 = init 42. 
let eq10_var2 = init 3. 
let eq10_output = StdOps.cos (StdOps.mul eq10_var1 eq10_var2)
let () = backward eq10_output
let ans_eq10 = get_grad eq10_var1

(* Setting up test for 3cos(3x)  *)
let eq11_var1 = init 42. 
let eq11_var2 = init 3. 
let eq11_output = StdOps.mul (StdOps.cos 
(StdOps.mul eq11_var1 eq11_var2)) eq11_var2
let () = backward eq11_output
let ans_eq11 = get_grad eq11_var1

(* Setting up test for (sinx)(cosx)  *)
let eq12_var1 = init 42. 
let eq12_output = StdOps.mul (StdOps.cos 
eq12_var1) (StdOps.sin eq12_var1) 
let () = backward eq12_output
let ans_eq12 = get_grad eq12_var1

(* Setting up test for (cosx)^4  *)
let eq13_var1 = init 42. 
let eq13_output = StdOps.pow (StdOps.cos eq13_var1) 4.
let () = backward eq13_output
let ans_eq13 = get_grad eq13_var1

(* Setting up test for (cosx)^2 + 4x + 4 *)
let eq14_var1 = init 42. 
let eq14_var2 = init 4. 
let eq14_output = StdOps.add (StdOps.add (StdOps.pow (StdOps.cos eq14_var1) 2.)
eq14_var2) (StdOps.mul eq14_var1 eq14_var2)
let () = backward eq14_output
let ans_eq14 = get_grad eq14_var1

let backward_tests = [
  make_backward_test "testing simple polynomial x*x" ans_x2 84.;
  make_backward_test "testing simple linear x" ans_x 1.;
  make_backward_test "testing medium polynomial x*x+4" ans_eq1 84.;
  make_backward_test "testing hard polynomial x*x+x+4" ans_eq2 85.;
  make_backward_test "testing small power x^4" ans_eq3 296352.;
  make_backward_test "testing medium power x^8" ans_eq5 1844314665984.;
  make_backward_test "testing large power x^15" ans_eq6 797225762616485973442560.;
  make_backward_test "testing simple sin x" ans_eq7 (-0.39998531498);
  make_backward_test "testing medium sin 3x" ans_eq8 2.83195241746;
  make_backward_test "testing complex 3 sin 3x" ans_eq4 8.49586;
  make_backward_test "testing simple cos x" ans_eq9 0.91652154791;
  make_backward_test "testing medium cos 3x" ans_eq10 (-0.98997247702);
  make_backward_test "testing complex 3 cos 3x" ans_eq11 (-2.96991743106);
  make_backward_test "testing complex combination 1 sinxcosx" 
  ans_eq12 (-0.68002349558);
  make_backward_test "testing complex combination 2 (cos x)^4" 
  ans_eq13 (-0.23460367);
  make_backward_test "testing complex combination 3 (cosx)^2 + 4x + 4" 
  ans_eq14 3.26680967993;
] *)

(** ADD COMMENTS!!!!!! The 
  equality only applies to float. *)
let matrix_eq mat1 mat2 : bool= 
  let ans = ref true in 
  if (Array.length mat1 <> Array.length mat2) ||
      (Array.length mat1.(0) <> Array.length mat2.(0)) 
      then false else (
      for i = 0 to ((Array.length mat1)-1) do 
          for j = 0 to ((Array.length mat1.(0))-1) do
          if not (approx_eq mat1.(i).(j) mat2.(i).(j)) then ans:=false else
          ()
          done
      done; 
      !ans )


(** [make_matrix_mul_test (name:string) (input1:mat) (input2:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.mat_mul input1 input2) expected_output]. *) 
let make_matrix_mul_test (name:string) (input1) (input2)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.mat_mul input1 input2) 
  expected_output) true)

let matrix_mul_tests = [
  make_matrix_mul_test "small size matrix with identity matrix" 
  [|[|22.03|]|] [|[|1.0|]|] [|[|22.03|]|] ;
  make_matrix_mul_test "complex multiplication test with small 
  size matrix" 
  [|[|88.93|]|] [|[|33.322|]|] [|[|2963.32546|]|] ;
  make_matrix_mul_test "medium size matrix with identity matrix" 
  [|[|42.42;10.0|];[|11.0;13.0|]|] [|[|1.0;0.0|];[|0.0;1.0|]|]
  [|[|42.42;10.0|];[|11.0;13.0|]|] ;
  make_matrix_mul_test "complex multiplication test with medium 
  size matrix" 
  [|[|1.0;1.0|];[|1.0;1.0|]|] [|[|1.0;1.0|];[|1.0;1.0|]|]
  [|[|2.0;2.0|];[|2.0;2.0|]|] ;
  make_matrix_mul_test "large size matrix with identity matrix" 
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] 
  [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|]
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] ;
  make_matrix_mul_test "complex multiplication test with large 
  size matrix" 
  [|[|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|]|]  
  [|[|10.0;11.0;12.0|];[|13.0;14.0;15.0|];[|16.0;17.0;18.0|]|]
  [|[|84.0;90.0;96.0|];[|201.0;216.0;231.0|];[|318.0;342.0;366.0|]|];
  make_matrix_mul_test "simple negative multiplication" 
  [|[|88.93|]|] [|[|-33.322|]|] [|[|-2963.32546|]|] ;
  make_matrix_mul_test "complex negative multiplication" 
  [|[|-1.0;-2.0;-3.0|];[|-4.0;-5.0;-6.0|];[|-7.0;-8.0;-9.0|]|]  
  [|[|-10.0;-11.0;-12.0|];[|-13.0;-14.0;-15.0|];[|-16.0;-17.0;-18.0|]|]
  [|[|84.0;90.0;96.0|];[|201.0;216.0;231.0|];[|318.0;342.0;366.0|]|];
  make_matrix_mul_test "small size matrix with 0 matrix" 
  [|[|212312312.03|]|] [|[|0.0|]|] [|[|0.0|]|] ;
  make_matrix_mul_test "large size matrix with 0 matrix" 
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] 
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|]
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|] ;
  (* "wrong verb exception" >:: (fun _-> *)
(* 
  assert_raises (InvalidDims) (make_matrix_mul_test "wrong size matrix" 
  [|[|1.0;1.0;1.0|];[|1.0;1.0;2.0|];[|3.0;2.0;1.0|]|] 
  [|[|1.0|]|]
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|])  ; *)
]

(** [make_matrix_add_test (name:string) (input1:mat) (input2:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.mat_add input1 input2) expected_output]. *) 
let make_matrix_add_test (name:string) (input1) (input2)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.mat_add input1 input2) 
  expected_output) true)

let matrix_add_tests = [
  make_matrix_add_test "small size matrix with 0 matrix" 
  [|[|22.03|]|] [|[|0.0|]|] [|[|22.03|]|] ;
  make_matrix_add_test "complex addition test with small 
  size matrix" 
  [|[|88.93|]|] [|[|33.322|]|] [|[|122.252|]|] ;
  make_matrix_add_test "medium size matrix with 0 matrix" 
  [|[|42.42;10.0|];[|11.0;13.0|]|] [|[|0.0;0.0|];[|0.0;0.0|]|]
  [|[|42.42;10.0|];[|11.0;13.0|]|] ;
  make_matrix_add_test "complex addition test with medium 
  size matrix" 
  [|[|1.0;1.0|];[|1.0;1.0|]|] [|[|1.0;1.0|];[|1.0;1.0|]|]
  [|[|2.0;2.0|];[|2.0;2.0|]|] ;
  make_matrix_add_test "large size matrix with 0 matrix" 
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] 
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|]
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] ;
  make_matrix_add_test "complex addition test with large 
  size matrix" 
  [|[|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|]|]  
  [|[|10.0;11.0;12.0|];[|13.0;14.0;15.0|];[|16.0;17.0;18.0|]|]
  [|[|11.0;13.0;15.0|];[|17.0;19.0;21.0|];[|23.0;25.0;27.0|]|];
  make_matrix_add_test "simple addition with negative numbers" 
  [|[|-88.93|]|] [|[|-33.322|]|] [|[|-122.252|]|] ;
  make_matrix_add_test "complex addition with negative numbers" 
  [|[|1.0;-2.0;3.0|];[|-4.0;5.0;6.0|];[|7.0;8.0;-9.0|]|]  
  [|[|10.0;-11.0;12.0|];[|-13.0;14.0;15.0|];[|16.0;17.0;-18.0|]|]
  [|[|11.0;-13.0;15.0|];[|-17.0;19.0;21.0|];[|23.0;25.0;-27.0|]|];
  (* ADD EXCEPTION CHECKING *)
]

(** [make_matrix_neg_test (name:string) (input:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.mat_negate input) expected_output]. *) 
let make_matrix_neg_test (name:string) (input)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.mat_negate input) 
  expected_output) true)

let matrix_negate_tests = [
  make_matrix_neg_test "Negating simple 0 matrix" [|[|0.0|]|]
   [|[|0.0|]|];
   make_matrix_neg_test "Negating complex 0 matrix" 
    [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|]
    [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|];
    make_matrix_neg_test "Negating simple positive matrix" [|[|313.0|]|]
   [|[|-313.0|]|];
   make_matrix_neg_test "Negating simple negative matrix" [|[|-313.0|]|]
   [|[|313.0|]|];
   make_matrix_neg_test "Negating complex positive matrix" 
    [|[|10.0;11.0;12.0|];[|13.0;14.0;15.0|];[|16.0;17.0;18.0|]|]
    [|[|-10.0;-11.0;-12.0|];[|-13.0;-14.0;-15.0|];[|-16.0;-17.0;-18.0|]|];
   make_matrix_neg_test "Negating complex negative matrix" 
    [|[|-10.0;-11.0;-12.0|];[|-13.0;-14.0;-15.0|];[|-16.0;-17.0;-18.0|]|]
    [|[|10.0;11.0;12.0|];[|13.0;14.0;15.0|];[|16.0;17.0;18.0|]|];
   make_matrix_neg_test "Negating complex mixed matrix" 
     [|[|42.42;10.0|];[|-11.0;-13.0|]|]
     [|[|-42.42;-10.0|];[|11.0;13.0|]|];
]

(** [make_matrix_sub_test (name:string) (input1:mat) (input2:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.mat_sub input1 input2) expected_output]. *) 
let make_matrix_sub_test (name:string) (input1) (input2)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.mat_sub input1 input2) 
  expected_output) true)

let matrix_sub_tests = [
  make_matrix_sub_test "small size matrix with 0 matrix second" 
  [|[|22.03|]|] [|[|0.0|]|] [|[|22.03|]|] ;
  make_matrix_sub_test "small size matrix with 0 matrix first" 
  [|[|0.0|]|] [|[|22.03|]|] [|[|-22.03|]|] ;
  make_matrix_sub_test "complex sub test with small 
  size matrix" 
  [|[|88.93|]|] [|[|33.322|]|] [|[|55.608|]|] ;
  make_matrix_sub_test "medium size matrix with 0 matrix at end" 
  [|[|42.42;10.0|];[|11.0;13.0|]|] [|[|0.0;0.0|];[|0.0;0.0|]|]
  [|[|42.42;10.0|];[|11.0;13.0|]|] ;
  make_matrix_sub_test "2 equal matrices" 
  [|[|1.0;1.0|];[|1.0;1.0|]|] [|[|1.0;1.0|];[|1.0;1.0|]|]
  [|[|0.0;0.0|];[|0.0;0.0|]|] ;
  make_matrix_sub_test "large size matrix with 0 matrix at end" 
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] 
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|]
  [|[|42.42;10.0;21.0|];[|11.0;13.0;31.4|];[|11.0;13.0;31.4|]|] ;
  make_matrix_sub_test "complex sub test with large 
  size matrix" 
  [|[|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|]|]  
  [|[|10.0;11.0;12.0|];[|13.0;14.0;15.0|];[|16.0;17.0;18.0|]|]
  [|[|-9.0;-9.0;-9.0|];[|-9.0;-9.0;-9.0|];[|-9.0;-9.0;-9.0|]|];
  make_matrix_sub_test "simple sub with negative numbers" 
  [|[|-88.93|]|] [|[|33.322|]|] [|[|-122.252|]|] ;
  make_matrix_sub_test "complex addition with negative numbers" 
  [|[|1.0;-2.0;3.0|];[|-4.0;5.0;6.0|];[|7.0;8.0;-9.0|]|]  
  [|[|10.0;-11.0;12.0|];[|-13.0;14.0;15.0|];[|16.0;17.0;-18.0|]|]
  [|[|-9.0;9.0;-9.0|];[|9.0;-9.0;-9.0|];[|-9.0;-9.0;9.0|]|]  
  (* ADD EXCEPTION CHECKING *)
]

(** [make_matrix_scale_test (name:string) (const:float) (matrix:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.scale const matrix) expected_output]. *) 
let make_matrix_scale_test (name:string) (const) (matrix)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.scale const matrix) 
  expected_output) true)

let matrix_scale_tests = [
  make_matrix_scale_test "Scale with 0 matrix" 10.34 [|[|0.0|]|]
   [|[|0.0|]|];
   make_matrix_scale_test "Scale with nonzero complex matrix" 
    5.0 [|[|1.0;1.0;1.0|];[|1.0;1.0;1.0|];[|1.0;1.0;1.0|]|]
    [|[|5.0;5.0;5.0|];[|5.0;5.0;5.0|];[|5.0;5.0;5.0|]|];
    make_matrix_scale_test "Scale with nonzero simple matrix" 
    5.0 [|[|22.03|]|] [|[|110.15|]|] ;
    make_matrix_scale_test "Scale with nonzero simple negative matrix" 
    5.0 [|[|-22.03|]|] [|[|-110.15|]|] ;
    make_matrix_scale_test "Scale with nonzero simple matrix with neg scalar" 
    (-5.0) [|[|22.03|]|] [|[|-110.15|]|] ;
    make_matrix_scale_test "Scale with non-sqaure matrix" 
    (-5.0) [|[|22.03;1.0|]|] [|[|-110.15;-5.0|]|] ;
]

(** [make_matrix_map_test (name:string) (f:float->float) (matrix:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.map f matrix) expected_output]. *) 
let make_matrix_map_test (name:string) (f) (matrix)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.map f matrix) 
  expected_output) true)

let matrix_map_tests = [
  make_matrix_map_test "identity function with simple matrix" (fun x->x)
  [|[|1.0|]|] [|[|1.0|]|];
  make_matrix_map_test "identity function with complex matrix" (fun x->x)
  [|[|10.0;-11.0;12.0|];[|-13.0;14.0;15.0|];[|16.0;17.0;-18.0|]|]
  [|[|10.0;-11.0;12.0|];[|-13.0;14.0;15.0|];[|16.0;17.0;-18.0|]|];
  make_matrix_map_test "identity function with identity matrix" (fun x->x)
  [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|]
  [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|];
  make_matrix_map_test "identity function with 0 matrix" (fun x->x)
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|]
  [|[|0.0;0.0;0.0|];[|0.0;0.0;0.0|];[|0.0;0.0;0.0|]|];
  make_matrix_map_test "identity function with non-square matrix" (fun x->x)
  [|[|1.0;4.0|]|] [|[|1.0;4.0|]|];
  make_matrix_map_test "abs function with positive matrix" 
  (fun (x:float) -> if x >= 0.0 then x else -.x)
  [|[|1.0;1.0;1.0|];[|1.0;1.0;1.0|];[|1.0;1.0;1.0|]|]
  [|[|1.0;1.0;1.0|];[|1.0;1.0;1.0|];[|1.0;1.0;1.0|]|];
  make_matrix_map_test "abs function with negative matrix" 
  (fun (x:float) -> if x >= 0.0 then x else -.x)
  [|[|-1.0;-1.0;-1.0|];[|-1.0;-1.0;-1.0|];[|-1.0;-1.0;-1.0|]|]
  [|[|1.0;1.0;1.0|];[|1.0;1.0;1.0|];[|1.0;1.0;1.0|]|];
   make_matrix_map_test "abs function with mixed matrix" 
  (fun (x:float) -> if x >= 0.0 then x else -.x)
  [|[|-1.0;1.0;-1.0|];[|-1.0;1.0;1.0|];[|1.0;1.0;-1.0|]|]
  [|[|1.0;1.0;1.0|];[|1.0;1.0;1.0|];[|1.0;1.0;1.0|]|];
]

(** [make_matrix_transpose_test (name:string) (input:mat) 
  (expected_output:mat)]  constructs an OUnit test named [name] 
  that asserts the quality of [expected_output] with 
  [matrix_eq (Math.transpose input) expected_output]. *) 
let make_matrix_transpose_test (name:string) (input)
(expected_output) =
name >::(fun _ -> assert_equal (matrix_eq
  (Math.transpose input) 
  expected_output) true)

let matrix_transpose_tests = [
  make_matrix_transpose_test "transposing simple 0 matrix" [|[|0.0|]|]
   [|[|0.0|]|];
   make_matrix_transpose_test "transposing identity matrix" 
   [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|]
   [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|];
   make_matrix_transpose_test "transposing sqaure matrix" 
   [|[|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|]|]
   [|[|1.0;4.0;7.0|];[|2.0;5.0;8.0|];[|3.0;6.0;9.0|]|];
   make_matrix_transpose_test "transposing non-sqaure matrix" 
   [|[|0.0;1.0|]|]
   [|[|0.0|];[|1.0|]|];
   
]

let suite = 
 "test suite for A6"  >::: List.flatten [
    (* backward_tests; *)
    matrix_mul_tests;
    matrix_add_tests;
    matrix_negate_tests;
    matrix_sub_tests;
    matrix_scale_tests;
    matrix_map_tests;
    matrix_transpose_tests;
]

let _ = run_test_tt_main suite