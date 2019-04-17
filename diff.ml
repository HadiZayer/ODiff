
type var = {mutable value: float; children : var array; 
            op: var array -> float array; mutable cur_grad : float; mutable grad : float}

type model = {params: var list; forward: var -> var}

let get_value x = x.value

let get_children x = x.children

let get_op x = x.op

(** [get_cur_grad x] is the float stored in the cur_grad parameter of x *)
let get_cur_grad x = x.cur_grad

let get_grad x = x.grad

let init x = 
  let id _ = [||] in
  {value=x; children=[||]; op=id; cur_grad=1.0;grad=0.0}


let forward m x = 
  m.forward x

let model_params m =
  m.params (*TODO: decide where to locate this + add to mli*)


let backward arg0 : unit=
  let rec backward_helper arg : unit =
    let grads = arg.op arg.children in
    for i = 0 to Array.length arg.children - 1 do
      arg.children.(i).cur_grad <- grads.(i) *. arg.cur_grad;
      arg.children.(i).grad <- arg.children.(i).grad
                               +. arg.children.(i).cur_grad;
      backward_helper arg.children.(i);
      (* arg0.children.(i).cur_grad <- 1.0; *)
    done
  in
  arg0.cur_grad <- 1.0;
  arg0.grad <- arg0.grad +. arg0.cur_grad;
  backward_helper arg0


(** Input is in fully evaluated form *)
let print (input:string) : unit = 
  match (String.split_on_char ' ' input)with
  | "add"::"("::"mult"::arg1::arg2::")"::arg3::rest -> 
    print_endline arg1; print_string " "; print_endline "\\"; 
    print_string "  "; print_endline "*";
    print_string " /"; print_endline " \\"; print_string arg2; 
    print_endline "   \\"; print_endline "     +"; 
    print_endline "    /"; print_string "   "; print_endline arg3;
  | "add"::arg1::arg2::rest -> print_endline arg1; print_string " ";
    print_endline "\\"; print_string "  "; print_endline "+";
    print_string " "; print_endline "/"; print_endline arg2; 
  | "mult"::arg1::arg2::rest -> print_endline arg1; print_string " ";
    print_endline "\\"; print_string "  "; print_endline "*";
    print_string " "; print_endline "/"; print_endline arg2; 
  | [single_char] -> print_endline single_char;
  | _ -> failwith "Error Invalid Input"


module StdOps = struct
  let add arg0 arg1 =
    let add_grad children = 
      assert (Array.length children = 2);
      [|1.0;1.0|] in
    let v = arg0.value +. arg1.value in
    {value=v; children=[|arg0;arg1|]; op=add_grad; cur_grad=1.0; grad=0.0}


  let mul arg0 arg1 =
    let mul_grad children = 
      assert (Array.length children = 2);
      [|children.(1).value; children.(0).value|] in
    let v=arg0.value *. arg1.value in
    {value=v; children=[|arg0; arg1|]; op=mul_grad; cur_grad=1.0;grad=0.0}

  let pow arg0 fl =
    let pow_grad children =
      assert (Array.length children = 1);
      [|fl *. (children.(0).value ** (fl -. 1.0))|] in
    let v = arg0.value ** fl in
    {value=v; children=[|arg0|]; op=pow_grad; cur_grad=1.0;grad=0.0}

  let sin arg0 =
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
    {value=v; children=[|arg0|]; op=cos_grad; cur_grad=1.0;grad=0.0}

end

module Model = struct
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

  let gd params lr =
    let step_grad () =
      let rec grad_helper = 
        function
        | [] -> ()
        | h::t -> h.value <- h.value -. lr *. h.grad; grad_helper t
      in grad_helper params in
    {step=step_grad; params=params}

  let step optimizer = optimizer.step()

  let zero_grad optimizer =
    let rec zero_grad_helper = function
      | [] -> ()
      | h::t -> h.grad <- 0.0; zero_grad_helper t
    in zero_grad_helper optimizer.params
end

