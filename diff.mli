(** The [Diff] module calculates partial derivatives between different variables
    represented by the var type through the use of backpropagation.

    The value of a variable is stored in the value parameter.
    The children variables that are used to create this variable through the 
    given operation are stored as an array in the children parameter. If the 
    variable is given (i.e. not the result of a operation), the children 
    parameter stores a empty array.
    The function to be used when differentiating during backpropagation based on
    the operation used to produce this variable is stored in the op parameter.
    The incremental gradiant used while calculating the final gradiant during 
    backpropagation is stored in the cur_grad parameter of the variable in the 
    output variable's children list.
    The final gradient calculated during backpropagation is stored in the grad
    parameter.

    To give a basic example, consider the derivative of x*x. First we would
    create the variable for x by calling init with any float value,

    let x = init 42.0;;

    Next, we create the equation using StdOps,

    let output = StdOps.mul x x;;

    Lastly, run the backwards propagation on the equation by calling backward,

    backward output;;

    Now if we check output we get,

{value = 1764.; children = [|{value = 42.; children = [||]; op = <fun>; 
  cur_grad = 1.; grad = 84.}; {value = 42.; children = [||]; op = <fun>; 
  cur_grad = 1.; grad = 84.}|]; op = <fun>; cur_grad = 1.; grad = 0.}

  If we look at the gradient of x, which can be either child as they are both x,
  the gradient is 84.0 which is 2 times the initial value of x.


*)

(** The abstract type representing a variable *)
type var

(* The abstract type of a model *)
type model 

(** [get_value x] is the float stored in the value parameter of x *)
val get_value : var -> float

(** [get_children x] is the var list stored in the children parameter of x *)
val get_children : var -> var array

(** [get_op x] is the function stored in the op parameter of x *)
val get_op : var -> var array -> float array

(** [get_value x] is the float stored in the grad parameter of x *)
val get_grad : var -> float

(** [init x] is a var record with a value of x, no children, an op function that
    yields an empty array, a cur_grad of 1.0, and a grad of 0.0 *)
val init : float -> var

val forward : model -> var -> var

(** [backward x] runs backwards propagation, with x as a starting point. *)
val backward : var -> unit

val model_params : model -> var list

val print : string -> unit

module StdOps : sig

  (** [add arg0 arg1] is the var record with a value of the sum of the values
      of arg0 and arg1, children of the array containing arg0 and arg1, op of a
      function that yields an array [1;1], a cur_grad of 1.0, and a grad of 0.0
  *)
  val add : var -> var -> var	

  val sub : var -> var -> var 

  (** [mul arg0 arg1] is the var record with a value of the product of the 
      values of arg0 and arg1, children of the array containing arg0 and arg1,
      op of a function that yields an array [arg1.value;arg2.value], a cur_grad
      of 1.0, and a grad of 0.0 *)
  val mul : var -> var -> var

  val pow : var -> float -> var

  val sin : var -> var

  val cos : var -> var
end

module Model : sig
  val linear_model : float -> float -> model
end

module Optim : sig
  type optim
  val step : optim -> unit
  val zero_grad : optim -> unit
  val gd : var list -> float -> optim
end

module Math : sig

  type mat = float array array
  exception InvalidDims

  (**[matmul a b] takes a matrix of size m x n and matrix of size n x z
   * returns the outer product of the matrices with size m x z
   * raises: InvalidDims if a's second dim doesn't agree with b second dim*)
  val mat_mul : mat -> mat -> mat

  (**[mat_add a b] takes two matrices of the same size and returns the sum
   * raises: InvalidDims if a and b don't have the same size*)  
  val mat_add : mat -> mat -> mat

  (**[mat_add a b] takes two matrices of the same size and returns the
   * subtraction
   * raises: InvalidDims if a and b don't have the same size*)
  val mat_sub : mat -> mat -> mat

  (**[scale c M] returns the matrix M scaled by c (cM)*)
  val scale : float -> mat -> mat 

  val map : ('a -> 'b) -> mat -> mat
end
