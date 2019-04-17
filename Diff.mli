
type diff (* var *)

(** [get_value x] is the float stored in the value parameter of x *)
val get_value : diff -> float

(** [get_children x] is the diff list stored in the children parameter of x *)
val get_children : diff -> diff list

(** [get_op x] is the function stored in the op parameter of x *)
val get_op : diff list -> float list

(** [get_cur_grad x] is the float stored in the cur_grad parameter of x *)
val get_cur_grad : diff -> float

(** [get_value x] is the float stored in the grad parameter of x *)
val get_grad : diff -> float

(** [init x] is a diff record with a value of x, no children, an op function that
    yields an empty array, a cur_grad of 0.0, and a grad of 0.0 *)
val init : float -> diff

(** [add arg0 arg1] is the diff record with a value of the sum of the values
    of arg0 and arg1, children of the array containing arg0 and arg1, op of a
    function that yields an array [1;1], a cur_grad of 0.0, and a grad of 0.0 *)
val add : diff -> diff -> diff

(** [add arg0 arg1] is the diff record with a value of the product of the values
    of arg0 and arg1, children of the array containing arg0 and arg1, op of a
    function that yields an array [arg1.value;arg2.value] a cur_grad of 0.0, 
    and a grad of 0.0 *)
val mul : diff -> diff -> diff

