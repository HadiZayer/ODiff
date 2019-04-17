
type var = {value: float; children : var array; 
            op: var array -> float array; mutable cur_grad : float; mutable grad : float}

let get_value x = x.value

let get_children x = x.children

let get_op x = x.op

let get_cur_grad x = x.cur_grad


let get_grad x = x.grad

let init x = 
  let id _ = [||] in
  {value=x; children=[||]; op=id; cur_grad=1.0;grad=0.0}

(** Input is in fully evaluated form *)

let rec backward arg0 : unit=
  let grads = arg0.op arg0.children in
  for i = 0 to Array.length arg0.children - 1 do
    arg0.children.(i).cur_grad <- grads.(i) *. arg0.cur_grad;
    arg0.children.(i).grad <- arg0.children.(i).grad
                              +. arg0.children.(i).cur_grad;
    backward arg0.children.(i);
    arg0.children.(i).cur_grad <- 1.0;
  done

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
    let add_grad children = [|1.0;1.0|] in
    let v = arg0.value +. arg1.value in
    {value=v; children=[|arg0;arg1|]; op=add_grad; cur_grad=1.0; grad=0.0}


  let mul arg0 arg1 =
    let mul_grad children = 
      assert (Array.length children = 2);
      [|children.(1).value; children.(0).value|] in
    let v=arg0.value *. arg1.value in
    {value=v; children=[|arg0; arg1|]; op=mul_grad; cur_grad=1.0;grad=0.0}
end
