module Diff = struct
  type diff = {value: float; children : diff array; 
  op: diff array -> float array; mutable cur_grad : float; mutable grad : float}
  let get_value dif = dif.value
  let get_children dif = dif.children
  let get_op dif = dif.op
  let get_cur_grad dif = dif.cur_grad
  let add arg0 arg1 =
    let add_grad children = [|1.0;1.0|] in
    let v = arg0.value +. arg1.value in
    {value=v; children=[|arg0;arg1|]; op=add_grad; cur_grad=0.0; grad=0.0}
  let mul arg0 arg1 =
    let mul_grad children = 
      assert (Array.length children = 2);
      [|children.(1).value; children.(0).value|] in
    let v=arg0.value *. arg1.value in
    {value=v; children=[|arg0; arg1|]; op=mul_grad; cur_grad=0.0;grad=0.0}
  let init x = 
    let id _ = [||] in
    {value=x; children=[||]; op=id; cur_grad=0.0;grad=0.0}
  let print = failwith "Unimplemented"
  end
