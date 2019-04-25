(*loads csv of floats and create a float list list*)
let load_csv filename = 
  let csv_array = Csv.load filename |> Csv.to_array in (*string array array*)
  let float_array = Array.map (Array.map float_of_string) csv_array
  in float_array in

let img_size = 784 in
let hidden_size1 = 64 in
let hidden_size2 = 32 in
let num_classes = 10 in

let lr = 0.005 in 

let print_matrix_size mat =
    print_string "rows "; print_int (Array.length mat);
    print_string " cols "; print_int (Array.length mat.(0));
    print_newline() in

let l1 = Diff.Layers.linear img_size hidden_size1 in
let l2 = Diff.Layers.linear hidden_size1 hidden_size2 in
let l3 = Diff.Layers.linear hidden_size2 num_classes in

let b1 = Diff.init (Diff.Math.mat_random 1 hidden_size1) in
let b2 = Diff.init (Diff.Math.mat_random 1 hidden_size2) in

let feed x =
    let y = Diff.StdOps.(sigmoid (add b1 (Diff.Layers.forward l1 x))) in
    let z = Diff.StdOps.(sigmoid (add b2 (Diff.Layers.forward l2 y))) in
     Diff.StdOps.sigmoid(Diff.Layers.forward l3 z) in

let params = (Diff.Layers.params l1) @ (Diff.Layers.params l2) @ (Diff.Layers.params l3) @ [b1 ; b2] in

let optim = Diff.Optim.gd params lr in


let sum mat =
    let counter = ref 0.0 in
    for i = 0 to (Array.length mat) - 1 do
        for j = 0 to (Array.length mat.(0)) - 1 do
            counter.contents <- (counter.contents +. mat.(i).(j))
        done
    done; counter.contents in

let largest (mat:float array array) :int =
    let max_idx = ref 1 in
    let max_val = ref (-1.0) in
    for i = 0 to (Array.length mat) - 1 do
        for j = 0 to (Array.length mat.(0)) - 1 do
            if max_val.contents < mat.(i).(j) then (
            max_val.contents <- mat.(i).(j);
            max_idx.contents <- j)
            else ();
        done
    done; max_idx.contents in

let create_one_hot c n = 
    let output = Array.make_matrix 1 n 0.0 in
    output.(0).(c) <- 1.0; output in

let one_hot_vectors = Array.make 10 (Diff.init [|[||]|]) in


for idx = 0 to 9 do
    one_hot_vectors.(idx) <- Diff.init (create_one_hot idx num_classes)
done;


(*executes a single epoch on the data and
 * perform gradient descent on the MSE Loss*)
let rec single_epoch  optim data target_idx =
    let x = Diff.init data in
    let target = one_hot_vectors.(target_idx) in (*one hot*)
    let output = feed x in

    let mse = Diff.StdOps.(pow (sub output target) 2.0) in
    Diff.Optim.zero_grad optim;
    Diff.backward mse;
(*     print_string "predicted "; print_int (largest (Diff.get_value output));
    print_string " target "; print_int (largest (Diff.get_value target));
    print_newline();

    print_string "loss "; print_float (sum (Diff.get_value mse)); print_newline(); *)
    Diff.Optim.step optim in

(*executes a single epoch on the data and
 * perform gradient descent on the MSE Loss*)
let rec test_epoch  optim data target_idx counter =
    let x = Diff.init data in
    (* let target = one_hot_vectors.(target_idx) in (*one hot*) *)
    let output = feed x in

    (* let mse = Diff.StdOps.(pow (sub output target) 2.0) in *)
    (* Diff.Optim.zero_grad optim; *)
    (* Diff.backward mse; *)
    let predicted = largest (Diff.get_value output) in
    print_string "predicted "; print_int (predicted);
    print_string " target "; print_int (target_idx);
    print_newline();

    if (target_idx = predicted) then
        counter.contents <- counter.contents + 1
    else ()
    (* print_string "loss "; print_float (sum (Diff.get_value mse)); print_newline(); *)
    in


(*performs [epohcs] iteration on the dataset and
 * prints the model info after each iteration*)
let rec train epochs data =
    for epoch = 1 to epochs do
        for i = 0 to (Array.length data) - 1 do
              let target_idx = (int_of_float data.(i).(0)) in
              let data_input = [|(Array.sub (data.(i)) 1 img_size)|] in
              single_epoch optim data_input target_idx;
        done;
      print_endline ("training epoch " ^ (string_of_int epoch)); 
    done in

let rec test epochs data =
    let counter = ref 0 in
    for i = 0 to (Array.length data) - 1 do
          let target_idx = (int_of_float data.(i).(0)) in
          let data_input = [|(Array.sub (data.(i)) 1 img_size)|] in
          test_epoch optim data_input target_idx counter;
    done; print_float ((float_of_int (counter.contents)) /. (float_of_int (Array.length data))) in
      (* print_endline ("epoch " ^ string_of_int epoch); *)

let train_data = load_csv "mnist_train.csv" in
train 10 train_data;
let test_data = load_csv "mnist_test.csv" in
test 1 test_data 