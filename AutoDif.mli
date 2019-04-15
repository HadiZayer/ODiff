module type Diff = sig
  type 'a diff (* var *)
  val get_value : 'a dif -> float
  val get_children : 'a dif -> 'a dif list
  val get_op : 'a dif list -> float list
  val get_cur_grad : 'a dif -> float
  val get_grad : 'a dif -> float
  end