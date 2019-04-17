
type diff (* var *)
val get_value : diff -> float
val get_children : diff -> diff array
val get_op : diff -> diff array -> float array
val get_cur_grad : diff -> float
val get_grad : diff -> float
val init : float -> diff
(* val add : 
val mul : 
val create_var : 
val print : 
*)

module StdOps : sig
	val add : diff -> diff -> diff
	val mul : diff -> diff -> diff
end
