
type diff (* var *)
val get_value : diff -> float
val get_children : diff -> diff list
val get_op : diff list -> float list
val get_cur_grad : diff -> float
val get_grad : diff -> float
val init : float -> diff
(* val add : 
val mul : 
val create_var : 
val print : 
*)
