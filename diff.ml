type mat = (float array) array

type var = {mutable value: mat; children : var array; 
            op: var array -> mat -> mat array; mutable cur_grad : mat; mutable grad : mat}

type model = {params: var list; forward: var -> var}

module Math = struct
  exception InvalidDims

  let mat_mul (m0:mat) (m1:mat) : mat =
    if Array.length m0.(0) <> Array.length m1
    then raise InvalidDims;
    let output = Array.make_matrix (Array.length m0) (Array.length m1.(0)) 0.0 in 
    for a=0 to (Array.length m0 - 1) do
      for b=0 to (Array.length m1.(0) - 1) do
        for c=0 to (Array.length m1 - 1) do
          output.(a).(b) <-  output.(a).(b) +. (m0.(a).(c) *. m1.(c).(b))
        done
      done
    done; 
    output

  let mat_add (mat1:mat) (mat2:mat):mat = 
    if (Array.length mat1 <> Array.length mat2) ||
       (Array.length mat1.(0) <> Array.length mat2.(0)) 
    then raise InvalidDims 
    else
      let mat3 = Array.make_matrix (Array.length mat1)
          (Array.length mat1.(0)) 0.0 in
      for i = 0 to ((Array.length mat1)-1) do 
        for j = 0 to ((Array.length mat1.(0))-1) do
          mat3.(i).(j) <- mat2.(i).(j) +. mat1.(i).(j);
        done
      done 
              ; mat3

  let mat_negate (mat1:mat):mat = 
    let output = Array.make_matrix (Array.length mat1)
        (Array.length mat1.(0)) 0.0 in
    for i = 0 to ((Array.length mat1)-1) do 
      for j = 0 to ((Array.length mat1.(0))-1) do
        output.(i).(j) <- -. mat1.(i).(j);
      done
    done 
            ; output

  let add_in_place (mat1:mat) (mat2:mat) : unit = 
    if (Array.length mat1 <> Array.length mat2) ||
       (Array.length mat1.(0) <> Array.length mat2.(0)) 
    then raise InvalidDims 
    else
      for i = 0 to ((Array.length mat1)-1) do 
        for j = 0 to ((Array.length mat1.(0))-1) do
          mat1.(i).(j) <- mat2.(i).(j) +. mat1.(i).(j);
        done
      done 

  let mat_sub (mat1:mat) (mat2:mat):mat = 
    if (Array.length mat1 <> Array.length mat2) ||
       (Array.length mat1.(0) <> Array.length mat2.(0)) 
    then raise InvalidDims 
    else
      let mat3 = Array.make_matrix (Array.length mat1)
          (Array.length mat1.(0)) 0.0 in
      for i = 0 to ((Array.length mat1)-1) do 
        for j = 0 to ((Array.length mat1.(0))-1) do
          mat3.(i).(j) <- mat1.(i).(j) -. mat2.(i).(j);
        done
      done 
              ; mat3

  let scale (const:float) (mat1:mat) :mat = 
    let mat2 = Array.make_matrix (Array.length mat1)
        (Array.length mat1.(0)) 0.0 in
    for i = 0 to ((Array.length mat1)-1) do 
      for j = 0 to ((Array.length mat1.(0))-1) do
        mat2.(i).(j) <- mat1.(i).(j) *. const;
      done
    done 
            ; mat2

  let map (f:float->float) (mat1:mat):mat = 
    let mat2 = Array.make_matrix (Array.length mat1)
        (Array.length mat1.(0)) 0.0 in
    for i = 0 to ((Array.length mat1)-1) do 
      for j = 0 to ((Array.length mat1.(0))-1) do
        mat2.(i).(j) <- f mat1.(i).(j);
      done
    done 
            ; mat2

  let map2 (f:float->float->float) (mat1:mat) (mat2:mat):mat = 
    (*TODO: add dimension check*)
    let mat3 = Array.make_matrix (Array.length mat1)
        (Array.length mat1.(0)) 0.0 in
    for i = 0 to ((Array.length mat1)-1) do 
      for j = 0 to ((Array.length mat1.(0))-1) do
        mat3.(i).(j) <- f (mat1.(i).(j)) (mat2.(i).(j));
      done
    done 
            ; mat3

  let transpose (mat1:mat) : mat = 
    let mat2 = Array.make_matrix (Array.length mat1.(0))
        (Array.length mat1) 0.0 in
    for i = 0 to ((Array.length mat1)-1) do 
      for j = 0 to ((Array.length mat1.(0))-1) do
        mat2.(j).(i) <- mat1.(i).(j);
      done
    done 
            ; mat2

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


let forward m x = 
  m.forward x

let model_params m =
  m.params (*TODO: decide where to locate this + add to mli*)


let backward arg0 : unit=
  let rec backward_helper arg : unit =
    let grads = arg.op arg.children arg.cur_grad in
    for i = 0 to Array.length arg.children - 1 do
      arg.children.(i).cur_grad <- grads.(i);
      Math.add_in_place arg.children.(i).grad arg.children.(i).cur_grad;
(*       arg.children.(i).grad <- arg.children.(i).grad
                               +. arg.children.(i).cur_grad; *)
                               (*TODO: rewrite*)

      backward_helper arg.children.(i);
      (* arg0.children.(i).cur_grad <- 1.0; *)
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

  let sigmoid arg0 =
    let e = 2.71828182845904 (*~approximately*) in
    let eval_value x = (1.) /. (1. +. e ** (-. x)) in
    (*TODO: figure out a better way to do it*)
    let grad_eval x out_grad = ((eval_value x) *. (1. -. (eval_value x))) *. out_grad in
    create_eltwise_op eval_value grad_eval arg0

(*   let sin arg0 =
    let sin_grad children =
      assert (Array.length children = 1);
      [|Pervasives.cos children.(0).value|] in
    let v = Pervasives.sin arg0.value in
    {value=v; children=[|arg0|]; op=sin_grad; cur_grad=1.0;grad=0.0}

  let cos arg0 =
    let cos_grad children =
      assert (Array.length children = 1);
      [|-. Pervasives.sin children.(0).value|] in
    let v = Pervasives.cos arg0.value in
    {value=v; children=[|arg0|]; op=cos_grad; cur_grad=1.0;grad=0.0} *)

end

(* module Model = struct
  let linear_model slope offset =
    let w = init slope in
    let b = init offset in
    let params = [w;b] in
    let forward x =
      StdOps.(add (mul w x) b)
    in
    {params=params; forward=forward}
end

module Optim = struct
  type optim = {step: unit -> unit; params: var list}

  let gd params lr = () (*TODO: fix*)
    let step_grad () =
      let rec grad_helper = 
        function
        | [] -> ()
        | h::t -> h.value <- h.value -. lr *. h.grad; grad_helper t
      in grad_helper params in
    {step=step_grad; params=params}

  let step optimizer = optimizer.step()

  let zero_grad optimizer = () (*TODO: fix*)
(*     let rec zero_grad_helper = function
      | [] -> ()
      | h::t -> h.grad <- 0.0; zero_grad_helper t
    in zero_grad_helper optimizer.params *)
end *)

