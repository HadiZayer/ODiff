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

(** The abstract type representing a variable. *)
type var

(** The abstract type of a model. *)
type model 

(** The abstract type representing a matrix. *)
type mat = float array array

(** [get_value x] is the float stored in the value parameter of x. *)
val get_value : var -> mat

(** [get_children x] is the var list stored in the children parameter of x. *)
val get_children : var ->  var array

(** [get_op x] is the function stored in the op parameter of x. *)
val get_op : var -> var array -> mat -> mat array

(** [get_value x] is the float stored in the grad parameter of x. *)
val get_grad : var -> mat

(** [init x] is a var record with a value of x, no children, an op function that
 *  yields an empty array, a cur_grad of 1.0, and a grad of 0.0. *)
val init : mat -> var

(** [backward arg0] runs backwards propagation, with arg0 as a starting point. 
*)
val backward : var -> unit

(** [create_eltwise_op val_eval_f grad_eval_f] evaluates each element of a 
 *  matrix using the function [val_eval_f] and using [grad_eval_f] to calculate
 *  the gradient of that element.*)
val create_eltwise_op : (float -> float) -> (float -> float -> float) -> 
  (var -> var)


(** [StdOps] is the module containing operations which can be used on variables,
 *  in order to create new variables which can be used in backwards 
 *  differentiation. *)
module StdOps : sig

  (** [add arg0 arg1] is the var record with a value of the sum of the values
   *  of arg0 and arg1, children of the array containing arg0 and arg1, op of a
   *  function that yields an array [|1.0;1.0|], a cur_grad of an array of the 
   *  same size as value containing 1.0's, and a grad of the same size 
   *  containing 0.0's.
  *)
  val add : var -> var -> var	

  (** [add arg0 arg1] is the var record with a value of the subraction of the
   *  value of arg1 from arg0, children of the array containing arg0 and arg1, 
   *  op of a function that yields an array [|1.0;-1.0|], a cur_grad of an array
   *  of the same size as value containing 1.0's, and a grad of the same size 
   *  containing 0.0's. *)
  val sub : var -> var -> var 

  (** [mul arg0 arg1] is the var record with a value of the product of the 
   *  values of arg0 and arg1, children of the array containing arg0 and arg1,
   *  op of a function that yields an array [|arg1.value;arg2.value|], a
   *  cur_grad of an array of the same size as value containing 1.0's,
   *  and a grad of the same size containing 0.0's. *)
  val mul : var -> var -> var

  (** [pow arg0 fl] is the var record with a value of arg0 raised to fl, 
   *  children of the array containing arg0, op of a function that yields an 
   *  array [|fl*.arg0.value**(fl -. 1.0)|], a cur_grad of an array of the same
   *  size as value containing 1.0's, and a grad of the same size containing 
   *  0.0's.*)
  val pow : var -> float -> var 

  (** [sigmoid arg0] is the var record with the value of arg0 inputed into the
   *  sigmoid function element wise, children of the array containing arg0, op 
   *  of a function that applies the derivative of the sigmoid function element 
   *  wise, a cur_grad of an array of the same size as value containing 1.0's,
   *  and a grad of the same size containing 0.0's. *)
  val sigmoid : var -> var

  (** [relu arg0] is the var record with the value of arg0 inputed into the
   *  relu (rectified linear units) function element wise, children of the array
   *  containing arg0, op of a function that applies the derivative of the relu 
   *  function element wise, a cur_grad of an array of the same size as value 
   *  containing 1.0's, and a grad of the same size containing 0.0's. *)
  val relu : var -> var

end

(** [Math] is the module containing functions which can be used on the mat type
 *  in order to either produce new matrices, or modify existing ones. *)
module Math : sig

  (** [InvalidDims] is an exception that occurs when the dimensions of the 
   *  matrices for a particular function are not the dimensions that are 
   *  expeceted. For example in addition of matrices the two input matrices have
   *  to have the same dimenstions, otherwise InvalidDims is called. *)
  exception InvalidDims

  (**[mat_mul mat0 mat1] takes a matrix of size m x n and matrix of size n x z
   * returns the outer product of the matrices with size m x z.
   * raises: InvalidDims if mat0's second dim doesn't agree with mat1 second 
   * dim. *)
  val mat_mul : mat -> mat -> mat

  (**[mat_add mat0 mat1] takes two matrices of the same size and returns the 
   * sum.
   * raises: InvalidDims if mat0 and mat1 don't have the same size. *)  
  val mat_add : mat -> mat -> mat

  (**[mat_add mat0 mat1] takes two matrices of the same size and returns the 
   * sum. The difference between this function and [mat_add] is that we alter
   * one of the given matrices rather than create a new one and thus we output
   * a unit.
   * raises: InvalidDims if mat0 and mat1 don't have the same size. *)  
  val add_in_place : mat -> mat -> unit

  (**[mat_add mat0 mat1] takes two matrices of the same size and returns the
   * subtraction.
   * raises: InvalidDims if mat0 and mat1 don't have the same size. *)  
  val mat_sub : mat -> mat -> mat

  (**[mat_negate mat0] takes one matrix  and returns new matrix with 
   * the negated values of mat0. *)
  val mat_negate : mat -> mat

  (**[scale c M] returns the matrix M scaled by c (cM). *)
  val scale : float -> mat -> mat 

  (** [map f mat0] returns a new matrix which is the result of applying function
   *  f to every value contained in the matrix mat0. *)
  val map : (float -> float) -> mat -> mat

  (** [map f mat0 mat1]
    * f: takes two floats and output a float
    * mat0: float matrix of size n x m
    * mat1: float matrix of size n x m
    * returns: a new matrix of size n x m that is the result of applying
    * f on each pair of mat0 & mat1 (i.e. f mat0.(i)(j) mat1.(i)(j)) for
    * all i < n, j < m 
    * raises: InvalidDims if mat0 and mat1 don't have the same size. *)
  val map2 : (float -> float -> float) -> mat -> mat -> mat

  (** [transpose mat0] returns the transpose of mat0. *)
  val transpose : mat -> mat

end


(** The Layers module essentially generates layers that can be used as 
 * part of the module.
 * currently it implements linear layers
 * potential layers to add in the future:
 * - convolution layer
 * - max_pool layer
 * - RNN layer *)
module Layers : sig

  (** Layer abstract type. *)
  type layer 

  (** [linear n m] creates a linear layer with weights matrix W of size n x m
   * the forward of the layer (with argument x) is simply Wx. *)
  val linear : int -> int -> layer

  (** [forward l x] applies the layer l on variable x, and
   *  generate an output var. *)
  val forward : layer -> var -> var

  (** [params l] returns list of the parameters used in layer l. *)
  val params : layer -> var list

end

(** The Optim module contains optimizers to update the model's parameters
  * currently it implements basic gradient descent
  * potential optimizers to add in the future:
  * - Adam
  * - SGD *)
module Optim : sig

  (** Optimizer abstract type. *)
  type optim

  (**[step optm] performs a single step with optimizer optm. *)
  val step : optim -> unit

  (**[zero_grad optm] zeros out the gradients for all parameters
    * optm is optimizing over. *)
  val zero_grad : optim -> unit

  (**[gd params lr] generates a classic gradient descent optimizer
    * that optimizes over the variables in params with learning rate lr. *)
  val gd : var list -> float -> optim
end
