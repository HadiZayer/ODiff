let load_csv filename = 
  let csv_list_array = Csv.load filename |> Csv.to_array |> Array.to_list in
  let csv_list = List.fold_right (fun x acc -> (Array.to_list x)::acc) csv_list_array [] in
  let float_list = List.map (List.map float_of_string) csv_list
  in float_list

let uniform_random mean range =
  Random.float range -. (range /. 2.) +. mean

let slope = uniform_random 0.0 1.0
let offset = uniform_random 0.0 1.0
let model = Diff.Model.linear_model slope offset
let optimizer = Diff.Optim.gd (Diff.model_params model) 0.001

let print_model_info model =
      let w = Diff.get_value (List.nth (Diff.model_params model) 0)
      in print_string "w: ";
      Printf.printf "%0.4f" w; print_newline ();
      let b = Diff.get_value (List.nth (Diff.model_params model) 1)
      in print_string "b: ";
      Printf.printf "%0.4f" b; print_newline ()

let rec single_epoch model optim data =
    match data with
    | [] -> ()
    | [x;y]::t -> 
        let data = Diff.init x in
        let target = Diff.init y in
        let output = Diff.forward model data in
        let mse = Diff.StdOps.(pow (sub output target) 2.0) in
        Diff.Optim.zero_grad optim;
        Diff.backward mse;
        Diff.Optim.step optim;
        single_epoch model optim t
    | _ -> failwith "error in given data"


let rec train epochs data =
    for i = 1 to epochs do
      single_epoch model optimizer data;
      print_endline ("epoch " ^ string_of_int i);
      print_model_info model
    done
    




let main = 
print_endline "initial model params:";
print_model_info model;
"points.csv" |> load_csv |> train 10;
