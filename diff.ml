type mat = (float array) array

type var = {mutable value: mat; children : var array; 
            op: var array -> mat -> mat array; mutable cur_grad : mat; 
            mutable grad : mat}

type model = {params: var list; forward: var -> var}

module Math = struct
  exception InvalidDims

  let mat_mul (mat0:mat) (mat1:mat) : mat =
    if Array.length mat0.(0) <> Array.length mat1
    then raise InvalidDims;
    let mat2 = Array.make_matrix (Array.length mat0) (Array.length mat1.(0)) 0.0
    in 
    for a=0 to (Array.length mat0 - 1) do
      for b=0 to (Array.length mat1.(0) - 1) do
        for c=0 to (Array.length mat1 - 1) do
          mat2.(a).(b) <-  mat2.(a).(b) +. (mat0.(a).(c) *. mat1.(c).(b))
        done
      done
    done
          ; mat2

  let mat_add (mat0:mat) (mat1:mat):mat = 
    if (Array.length mat0 <> Array.length mat1) ||
       (Array.length mat0.(0) <> Array.length mat1.(0)) 
    then raise InvalidDims 
    else
      let mat2 = Array.make_matrix (Array.length mat0)
          (Array.length mat0.(0)) 0.0 in
      for i = 0 to ((Array.length mat0)-1) do 
        for j = 0 to ((Array.length mat0.(0))-1) do
          mat2.(i).(j) <- mat1.(i).(j) +. mat0.(i).(j);
        done
      done 
              ; mat2

  let add_in_place (mat0:mat) (mat1:mat) : unit = 
    if (Array.length mat0 <> Array.length mat1) ||
       (Array.length mat0.(0) <> Array.length mat1.(0)) 
    then raise InvalidDims 
    else
      for i = 0 to ((Array.length mat0)-1) do 
        for j = 0 to ((Array.length mat0.(0))-1) do
          mat0.(i).(j) <- mat1.(i).(j) +. mat0.(i).(j);
        done
      done 

  let mat_sub (mat0:mat) (mat1:mat):mat = 
    if (Array.length mat0 <> Array.length mat1) ||
       (Array.length mat0.(0) <> Array.length mat1.(0)) 
    then raise InvalidDims 
    else
      let mat2 = Array.make_matrix (Array.length mat0)
          (Array.length mat0.(0)) 0.0 in
      for i = 0 to ((Array.length mat0)-1) do 
        for j = 0 to ((Array.length mat0.(0))-1) do
          mat2.(i).(j) <- mat0.(i).(j) -. mat1.(i).(j);
        done
      done 
              ; mat2

  let scale (const:float) (mat0:mat) :mat = 
    let mat1 = Array.make_matrix (Array.length mat0)
        (Array.length mat0.(0)) 0.0 in
    for i = 0 to ((Array.length mat0)-1) do 
      for j = 0 to ((Array.length mat0.(0))-1) do
        mat1.(i).(j) <- mat0.(i).(j) *. const;
      done
    done 
            ; mat1

  let mat_negate (mat0:mat):mat = 
    scale (-1.0) mat0

  let map (f:float->float) (mat0:mat):mat = 
    let mat1 = Array.make_matrix (Array.length mat0)
        (Array.length mat0.(0)) 0.0 in
    for i = 0 to ((Array.length mat0)-1) do 
      for j = 0 to ((Array.length mat0.(0))-1) do
        mat1.(i).(j) <- f mat0.(i).(j);
      done
    done 
            ; mat1

  let map2 (f:float->float->float) (mat0:mat) (mat1:mat):mat = 
    (*TODO: add dimension check*)
    let mat2 = Array.make_matrix (Array.length mat0)
        (Array.length mat0.(0)) 0.0 in
    for i = 0 to ((Array.length mat0)-1) do 
      for j = 0 to ((Array.length mat0.(0))-1) do
        mat2.(i).(j) <- f (mat0.(i).(j)) (mat1.(i).(j));
      done
    done; 
    mat2

  let transpose (mat0:mat) : mat = 
    let mat1 = Array.make_matrix (Array.length mat0.(0))
        (Array.length mat0) 0.0 in
    for i = 0 to ((Array.length mat0)-1) do 
      for j = 0 to ((Array.length mat0.(0))-1) do
        mat1.(j).(i) <- mat0.(i).(j);
      done
    done; 
    mat1

  let mat_random (n:int) (m:int): mat = 
    let mat0 = Array.make_matrix (n)(m) 0.0 in
    let uniform_random mean range =
      Random.float range -. (range /. 2.) +. mean in 
    for i = 0 to (n-1) do 
      for j = 0 to (m-1) do
        mat0.(i).(j) <- uniform_random 0.0 2.0;
      done
    done; 
    mat0

end

let get_value x = x.value

let get_children x = x.children

let get_op x = x.op

(** [get_cur_grad x] is the float stored in the cur_grad parameter of x *)
let get_cur_grad x = x.cur_grad

let get_grad x = x.grad

let init x = 
  let id _ _ = [||] in
  let rows = Array.length x in
  let cols = Array.length x.(0) in
  {value=x; children=[||]; op=id; 
   cur_grad = Array.make_matrix rows cols 1.0;
   grad = Array.make_matrix rows cols 0.0}


let backward arg0 : unit=
  let rec backward_helper arg : unit =
    let grads = arg.op arg.children arg.cur_grad in
    for i = 0 to Array.length arg.children - 1 do
      arg.children.(i).cur_grad <- grads.(i);
      Math.add_in_place arg.children.(i).grad arg.children.(i).cur_grad;
      backward_helper arg.children.(i);
    done
  in
  let rows = Array.length arg0.value in
  let cols = Array.length arg0.value.(0) in
  arg0.cur_grad <- Array.make_matrix rows cols 1.0;
  Math.add_in_place arg0.grad arg0.cur_grad;
  backward_helper arg0

let create_eltwise_op val_eval_f grad_eval_f =
  let operation arg0 = 
    let op_grad children out_grad =
      [|Math.map2 grad_eval_f children.(0).value out_grad|] in
    let v = Math.map val_eval_f (arg0.value) in
    let rows = Array.length v in
    let cols = Array.length v.(0) in
    {value=v; children=[|arg0|]; op=op_grad;
     cur_grad = Array.make_matrix rows cols 1.0;
     grad = Array.make_matrix rows cols 0.0} in
  operation


module StdOps = struct
  let add arg0 arg1 =
    let add_grad children out_grad = 
      assert (Array.length children = 2);
      [|out_grad;out_grad|] in

    let v = Math.mat_add arg0.value arg1.value in
    let rows = Array.length v in
    let cols = Array.length v.(0) in
    {value=v; children=[|arg0;arg1|]; op=add_grad;
     cur_grad = Array.make_matrix rows cols 1.0;
     grad = Array.make_matrix rows cols 0.0}

  let sub arg0 arg1 =
    let sub_grad children out_grad = 
      assert (Array.length children = 2);
      [|out_grad;Math.mat_negate out_grad|] in

    let v = Math.mat_sub arg0.value arg1.value in
    let rows = Array.length v in
    let cols = Array.length v.(0) in
    {value=v; children=[|arg0;arg1|]; op=sub_grad;
     cur_grad = Array.make_matrix rows cols 1.0;
     grad = Array.make_matrix rows cols 0.0}

  let mul arg0 arg1 =
    let mul_grad children out_grad = 
      assert (Array.length children = 2);
      let grad_0 = Math.mat_mul out_grad (Math.transpose children.(1).value) in
      let grad_1 = Math.mat_mul (Math.transpose children.(0).value) out_grad in
      [|grad_0; grad_1|] in
    let v= Math.mat_mul arg0.value arg1.value in
    let rows = Array.length v in
    let cols = Array.length v.(0) in
    {value=v; children=[|arg0; arg1|]; op=mul_grad;
     cur_grad = Array.make_matrix rows cols 1.0;
     grad = Array.make_matrix rows cols 0.0}


  let pow arg0 fl =
    let grad_eval x out_grad =
      fl *. (x ** (fl -. 1.0) *. out_grad) in
    let value_eval x = x ** fl in
    create_eltwise_op value_eval grad_eval arg0

  (*----------------non-linearities (activation-functions)----------------*)

  let sigmoid arg0 =
    let e = 2.71828182845904 (*~approximately*) in
    let eval_value x = (1.) /. (1. +. e ** (-. x)) in
    let grad_eval x out_grad = ((eval_value x) *. (1. -. (eval_value x))) *. 
                               out_grad in
    create_eltwise_op eval_value grad_eval arg0

  let relu arg0 =
    let eval_value x = if x > 0.0 then x else 0.0 in
    let grad_eval x out_grad = if x > 0.0 then out_grad else 0.0 in
    create_eltwise_op eval_value grad_eval arg0

end

module Layers = struct

  (*layer abstract type*)
  type layer = {params: var list; forward: var -> var}

  (** [linear n m] creates a linear layer with weights matrix W of size n x m
   * the forward of the layer (with argument x) is simply Wx
   * x has to have size m x b. output size is n x b *)
  let linear n m =
    let w = init (Math.mat_random n m) in
    let forward x = StdOps.mul x w in
    let params = [w] in
    {params=params; forward=forward}

  (** [forward l x] applies the layer l on variable x, and
      generate an output var *)
  let forward l x = l.forward x

  (** [params l] returns list of the parameters used in layer l*)
  let params l = l.params

end

module Optim = struct
  type optim = {step: unit -> unit; params: var list}

  let gd params lr = 
    let step_grad () =
      let rec grad_helper = 
        function
        | [] -> ()
        | h::t ->
          Math.add_in_place h.value (Math.scale (-.lr)  h.grad); grad_helper t
      in grad_helper params in
    {step=step_grad; params=params}

  let step optimizer = optimizer.step()

  let zero_grad optimizer = 
    let zero_matrix (matrix:mat) : unit =
      for i = 0 to ((Array.length matrix)-1) do 
        for j = 0 to ((Array.length matrix.(0))-1) do
          matrix.(i).(j) <- 0.0
        done
      done; ()
    in
    let rec zero_grad_helper = function
      | [] -> ()
      | h::t -> zero_matrix h.grad; zero_grad_helper t
    in zero_grad_helper optimizer.params
end

