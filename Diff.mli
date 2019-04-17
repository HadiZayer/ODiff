
type var (* var *)
type model (* model for parameters *)
(** [get_value x] is the float stored in the value parameter of x *)
val get_value : var -> float

(** [get_children x] is the var list stored in the children parameter of x *)
val get_children : var -> var array

(** [get_op x] is the function stored in the op parameter of x *)
val get_op : var -> var array -> float array

(** [get_cur_grad x] is the float stored in the cur_grad parameter of x *)
val get_cur_grad : var -> float

(** [get_value x] is the float stored in the grad parameter of x *)
val get_grad : var -> float

(** [init x] is a var record with a value of x, no children, an op function that
    yields an empty array, a cur_grad of 0.0, and a grad of 0.0 *)
val init : float -> var

module StdOps : sig
  (** [add arg0 arg1] is the var record with a value of the sum of the values
      of arg0 and arg1, children of the array containing arg0 and arg1, op of a
      function that yields an array [1;1], a cur_grad of 0.0, and a grad of 0.0 *)
  val add : var -> var -> var	
  (** [add arg0 arg1] is the var record with a value of the product of the values
      of arg0 and arg1, children of the array containing arg0 and arg1, op of a
      function that yields an array [arg1.value;arg2.value] a cur_grad of 0.0, 
      and a grad of 0.0 *)
  val mul : var -> var -> var
end

module Model : sig
  val linear_model : float -> float -> model
end

module Optim : sig
  type optim
  val step : optim -> unit
  val gd : var list -> float -> optim
end
