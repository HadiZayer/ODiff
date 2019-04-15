module Diff = struct
  type 'a diff = {value: float; children : 'a diff list; 
  op: float list; cur_grad : float; grad : float}
  let get_value dif = dif.value
  let get_children dif = dif.children
  let get_op dif = dif.op
  let get_cur_grad dif = dif.cur_grad
  let add = failwith "Unimplemented"
  let mul = failwith "Unimplemented"
  let create_var = failwith "Unimplemented"
  let print = failwith "Unimplemented"
  end
