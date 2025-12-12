let file_contents =
  In_channel.with_open_bin "day10.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

type machine = { lights : bytes; buttons : int array list; joltage : int array }

let machines =
  String.split_on_char '\n' file_contents
  |> List.map (fun line ->
      let parts = String.split_on_char ' ' line in
      let lights =
        List.hd parts |> fun display ->
        Bytes.init (String.length display - 2) (fun i -> display.[i + 1])
      in
      let buttons =
        List.drop 1 parts
        |> List.take_while (fun s ->
            String.contains s '(' || String.contains s ')')
        |> List.map (fun s ->
            let str = String.sub s 1 (String.length s - 2) in
            let nums =
              str |> String.split_on_char ','
              |> List.map (fun s -> int_of_string s)
            in
            Array.of_list nums)
      in
      let joltage =
        List.drop_while
          (fun s -> not (String.contains s '{' || String.contains s '}'))
          parts
        |> List.hd
        |> fun s ->
        let str = String.sub s 1 (String.length s - 2) in
        let nums =
          str |> String.split_on_char ',' |> List.map (fun s -> int_of_string s)
        in
        Array.of_list nums
      in
      { lights; buttons; joltage })

let print_button b =
  print_string "(";
  Array.iteri
    (fun i n ->
      Printf.printf (if i < Array.length b - 1 then "%d," else "%d") n)
    b;
  print_string ") "

let get_all_possible_button_presses buttons =
  let rec calc set buttons =
    match buttons with
    | [] -> [ set ]
    | b :: tl -> calc (b :: set) tl @ calc set tl
  in
  calc [] buttons

let push_buttons light_display_length sequence =
  let light = Bytes.init light_display_length (fun _ -> '.') in
  List.iter
    (fun button ->
      Array.iter
        (fun light_position ->
          let value = Bytes.get light light_position in
          Bytes.set light light_position (if value = '.' then '#' else '.'))
        button)
    sequence;
  light

let get_shortest_correct_sequence correct_light_display sequences =
  List.fold_left
    (fun acc sequence ->
      let display =
        push_buttons (Bytes.length correct_light_display) sequence
      in
      if display = correct_light_display then
        if acc = None then Some sequence
        else
          let acc = Option.get acc in
          if List.length sequence < List.length acc then Some sequence
          else Some acc
      else acc)
    None sequences
  |> Option.get

let first_part =
  List.fold_left
    (fun acc machine ->
      let sequences = get_all_possible_button_presses machine.buttons in
      let shortest = get_shortest_correct_sequence machine.lights sequences in
      acc + List.length shortest)
    0 machines

let () = Printf.printf "%d\n" first_part
let () = flush_all ()

let print_system system =
  Array.iter
    (fun (variables, jolt) ->
      Array.iter (fun v -> Printf.printf "%d " v) variables;
      Printf.printf "= %d" jolt;
      print_newline ())
    system

let get_system_equations machine =
  let num_variables = List.length machine.buttons in
  let joltage_length = Array.length machine.joltage in
  Array.init (max num_variables joltage_length) (fun i ->
      let variables = Array.init num_variables (fun _ -> 0) in
      if i < joltage_length then (
        let joltage_level = machine.joltage.(i) in
        List.iteri
          (fun idx_button button ->
            if Array.mem i button then variables.(idx_button) <- 1)
          machine.buttons;
        (variables, joltage_level))
      else (variables, 0))

let rec get_row_with_pivot_at_col system col =
  Array.find_mapi
    (fun idx (variables, _) ->
      let variables, _ = system.(idx) in
      let pivot = Array.find_index (fun v -> v <> 0) variables in
      if Option.is_some pivot && Option.get pivot = col then Some idx else None)
    system

let get_leading_non_zero_entry_idx system row =
  let variables, _ = system.(row) in
  let index_of_non_zero = Array.find_index (fun v -> v <> 0) variables in
  index_of_non_zero

let reorder_rows system =
  system |>
  Array.iteri
    (fun i (variables, _) ->
      let rec fold idx acc =
        if idx = Array.length system then acc
        else
          let variables, _ = system.(idx) in
          let lead_non_zero_entry =
            Array.find_index (fun v -> v <> 0) variables
          in
          let acc =
            match lead_non_zero_entry with
            | Some lead_non_zero_entry ->
                if lead_non_zero_entry = i then idx :: acc else acc
            | None -> acc
          in
          fold (idx + 1) acc
      in
      let rows_with_same_leading_entry_col = fold 0 [] in
      let sorted =
        List.sort
          (fun idx idx' ->
            let variables, _ = system.(idx) and variables', _ = system.(idx') in
            abs variables.(i) - abs variables'.(i))
          rows_with_same_leading_entry_col
        |> List.take 1
      in
      match sorted with
      | [] -> ()
      | pivot :: _ ->
          let temp = system.(i) in
          system.(i) <- system.(pivot);
          system.(pivot) <- temp)

let apply_row_addition_if_needed row system =
  let break = ref false in
  while
    begin match get_leading_non_zero_entry_idx system row with
    | Some index_of_non_zero -> index_of_non_zero < row
    | None -> false
    end
    && not !break
  do
    let index_of_non_zero =
      get_leading_non_zero_entry_idx system row |> Option.get
    in
    let row_with_pivot_at_col =
      get_row_with_pivot_at_col system index_of_non_zero
    in
    begin match row_with_pivot_at_col with
    | Some row_with_pivot_at_col ->
        if row_with_pivot_at_col <> row then
          let variables_used_to_cancel, joltage_level =
            system.(row_with_pivot_at_col)
          in
          let variables, joltage_lvl_row = system.(row) in
          let multiplier =
            -variables.(index_of_non_zero)
            / variables_used_to_cancel.(index_of_non_zero)
          in
          if
            abs variables.(index_of_non_zero)
            >= abs variables_used_to_cancel.(index_of_non_zero)
          then (
            Array.iteri
              (fun i v ->
                let v = v * multiplier in
                variables.(i) <- v + variables.(i))
              variables_used_to_cancel;
            system.(row) <-
              (variables, (multiplier * joltage_level) + joltage_lvl_row))
          else break := true
        else break := true
    | None -> failwith "ASSUMPTION WRONG"
    end;
    reorder_rows system
  done

let rec loop_and_create_proper_pivots system =
  Array.iteri
    (fun idx (variables, _) -> apply_row_addition_if_needed idx system)
    system

let to_row_echelon_form system = loop_and_create_proper_pivots system

let get_free_variables system =
  let num_variables = fst system.(0) |> Array.length in
  let rec fold acc idx =
    if idx = min (Array.length system) num_variables then
      acc
      @ List.init
          (max 0 (num_variables - Array.length system))
          (fun i -> i + idx)
    else
      let leading_non_zero = get_leading_non_zero_entry_idx system idx in
      fold
        (if
           Option.is_none leading_non_zero
           || Option.is_some leading_non_zero
              && Option.get leading_non_zero <> idx
         then idx :: acc
         else acc)
        (idx + 1)
  in
  fold [] 0 |> List.sort (fun a b -> a - b)

let plug_in system variable_values free_variable_indices =
  let num_variables = fst system.(0) |> Array.length in
  let rec reverse_iter idx =
    let variables, jolt_level = system.(idx) in
    let rec fold_left idx acc =
      if idx = Array.length variables then acc
      else
        let value = variable_values.(idx) * variables.(idx) in
        fold_left (idx + 1) (acc + value)
    in
    let sum = fold_left 0 0 in
    if not (List.mem idx free_variable_indices) then
      variable_values.(idx) <- (jolt_level - sum) / variables.(idx);
    if idx = 0 then () else reverse_iter (idx - 1)
  in
  reverse_iter (min (Array.length system - 1) (num_variables - 1));
  variable_values

let solve system =
  Array.iteri
    (fun i (variables, jolt_level) ->
      if jolt_level < 0 then (
        Array.iteri (fun i _ -> variables.(i) <- -1 * variables.(i)) variables;
        system.(i) <- (variables, -jolt_level)))
    system;
  let free_variables_indices = get_free_variables system in
  let num_variables = fst system.(0) |> Array.length in
  let free_variable_limits =
    Array.init num_variables (fun i ->
        Array.fold_left
          (fun acc (variables, jlt_lvl) ->
            if variables.(i) <> 0 then max acc jlt_lvl else acc)
          0 system)
  in
  let check variable_values =
    let variable_values =
      plug_in system variable_values free_variables_indices
    in
    let res =
      Array.for_all (fun v -> v >= 0) variable_values
      && Array.for_all
           (fun (variables, jolt_level) ->
             let sum = ref 0 in
             Array.iteri
               (fun i v -> sum := !sum + (variable_values.(i) * v))
               variables;
             jolt_level = !sum)
           system
    in
    res
  in
  let valid_variable_values = ref [] in
  let smallest = ref Int.max_int in
  let rec loop variable_values free_variables_indices' =
    match free_variables_indices' with
    | [] -> ()
    | free_index :: tl ->
        for v = 0 to free_variable_limits.(free_index) do
          if v <= !smallest then (
            variable_values.(free_index) <- v;
            loop variable_values tl;
            let valid = check variable_values in
            if valid then (
              let summed =
                Array.fold_left
                  (fun acc v ->
                    assert (v >= 0);
                    acc + v)
                  0 variable_values
              in
              smallest := min !smallest summed;
              valid_variable_values :=
                Array.copy variable_values :: !valid_variable_values);
            Array.iteri
              (fun i _ ->
                if not (List.mem i free_variables_indices) then
                  variable_values.(i) <- 0)
              variable_values)
        done;
        variable_values.(free_index) <- 0
  in
  let valid_variable_values =
    if free_variables_indices = [] then
      [ plug_in system (Array.init num_variables (fun _ -> 0)) [] ]
    else (
      loop (Array.init num_variables (fun _ -> 0)) free_variables_indices;
      !valid_variable_values)
  in
  assert (List.length valid_variable_values > 0);
  List.fold_left
    (fun acc variable_values ->
      if acc = None then Some variable_values
      else
        let sum = Array.fold_left (fun acc' v -> v + acc') 0 variable_values in
        let sum' =
          Array.fold_left (fun acc' v -> v + acc') 0 (Option.get acc)
        in
        if sum < sum' then Some variable_values else acc)
    None valid_variable_values
  |> Option.get

let second_part =
  List.fold_left
    (fun acc machine ->
      let system = get_system_equations machine in
      to_row_echelon_form system;
      let variable_values = solve system in
      let sum = Array.fold_left (fun acc v -> acc + v) 0 variable_values in
      acc + sum)
    0 machines

let () = Printf.printf "%d\n" second_part
(* 197 too low *)

(*
(3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
a b c d e f
0a+ 0b + 0c + 0d + e + f = 3
0a + b + 0c + 0d + 0e + f = 5
0a + 0b + c + d + e + 0f = 4
a + b + 0c + d + 0e + 0f = 7

a = 1
b = 3
c = 0
d = 3
e = 1
f = 2

a + b + d = 7
b + f = 5
c + d + e = 4
e + f = 3

free variables are d and f
*)

(*
[.###] (0,1) (3) (2) (1) {7,7,16,13}
a = 7
a + d = 7
c = 16
b = 13
*)

(*
[.##.#...] (0,4) (0,2,4,5,7) (0,1,3,4,5,7) (0,2,3,5,6,7) (1,2,6,7) (1,3) (0,6,7) (0,2,3,6,7) (0,7) (0,2,4,5,6) {265,33,227,40,229,215,224,76}

a b c d g h i j = 265
b d e h j = 227
c e f = 33
c d f h = 40
d e g h j = 224
a b c j = 229
b c d j = 215
b c d e g h i = 76

a b c d g h i j = 265
c e f = 33
b d e h j = 227
c d f h = 40
a b c j = 229
b c d j = 215
d e g h j = 224
b c d e g h i = 76

a b c j = 229
b d e h j = 227
c e f = 33
c d f h = 40
d g h i = 36
b c d j = 215
d e g h j = 224
b c d e g h i = 76

a b c j = 229
b d e h j = 227
c e f = 33
d -e h = -7
d g h i = 36
b c d j = 215
d e g h j = 224
b c d e g h i = 76

a b c j = 229
b d e h j = 227
c e f = 33
d -e h = -7
e g i = 36
b c d j = 215
d e g h j = 224
b c d e g h i = 76

a b c j = 229
b d e h j = 227
c e f = 33
d -e h = -7
e g i = 36
c -e -h = 215
d e g h j = 224
b c d e g h i = 76

a b c j
b d e h j
c e f
d -e h
e g i
-2e -f -h
d e g h j
b c d e g h i

a b c j
b d e h j
c e f
d -e h
e g i
-e -f g -h i
d e g h j
b c d e g h i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
d e g h j
b c d e g h i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
2e g j
b c d e g h i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
-2g -2i j
b c d e g h i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
-2g -2i j
c g i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
-2g -2i j
-e -f g i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
-2g -2i j
-f 2g 2i

a b c j
b d e h j
c e f
d -e h
e g i
-f 2g -h 2i
-2g -2i j
h
*)

(*

[#...#] (1,3) (2,3,4) (0,2,3) (0,1,2) (2,3) {37,24,60,50,16}
e = 7
d = 17
c = 20
b = 16
a = 7

a+d =24
b +c +d +e = 60
c + d = 37
-2d = -34
-e = -7

*)
