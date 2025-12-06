let file_contents =
  In_channel.with_open_bin "day6.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

type arr = Nums of int array | Ops of (int -> int -> int) array

let get_arr_type line =
  let parts = String.split_on_char ' ' line in
  let filtered = List.filter (fun p -> p <> "") parts in
  if List.exists (fun p -> p = "*" || p = "+") filtered then
    Ops
      (List.map
         (fun p -> if p = "*" then fun a b -> a * b else fun a b -> a + b)
         filtered
      |> Array.of_list)
  else Nums (List.map (fun p -> int_of_string p) filtered |> Array.of_list)

let rows = String.split_on_char '\n' file_contents |> List.map get_arr_type

let operations =
  List.drop_while (function Nums _ -> true | Ops _ -> false) rows |> List.hd

let nums = List.take_while (function Nums _ -> true | Ops _ -> false) rows

let first_part =
  let rec fold idx acc ops =
    match ops with
    | Nums _ -> failwith "impossible"
    | Ops ops ->
        if idx = Array.length ops then acc
        else
          let acc' =
            List.fold_left
              (fun acc' arr ->
                match arr with
                | Nums arr ->
                    let res = ops.(idx) arr.(idx) acc' in
                    if res = 0 && acc' = 0 && arr.(idx) <> 0 then
                      ops.(idx) arr.(idx) 1
                    else res
                | Ops _ -> failwith "impossible")
              0 nums
          in
          fold (idx + 1) (acc + acc') operations
  in
  fold 0 0 operations

let () = Printf.printf "%d\n" first_part

let matrix =
  String.split_on_char '\n' file_contents
  |> List.map (fun line -> Bytes.of_string line)
  |> Array.of_list

let max_column_idx = Array.fold_left (fun acc b -> max acc (Bytes.length b - 1)) 0 matrix

let second_part =
  let acc = ref 0 in
  let op = ref None in
  let inner_acc = ref 0 in
  for c = 0 to max_column_idx do
    let s = ref [] in
    for r = 0 to Array.length matrix - 1 do
      let char =
        if c >= Bytes.length matrix.(r) then ' ' else Bytes.get matrix.(r) c
      in
      match char with
      | '*' | '+' -> op := Some char
      | ' ' -> ()
      | _ -> s := char :: !s
    done;
    let str = List.fold_left (fun acc c -> String.make 1 c ^ acc) "" !s in
    let n = match int_of_string str with n -> n | exception e -> 0 in
    if n = 0 then (
      acc := !inner_acc + !acc;
      inner_acc := 0);
    if !op = Some '*' then
      inner_acc := n * if !inner_acc = 0 then 1 else !inner_acc;
    if !op = Some '+' then inner_acc := n + !inner_acc
  done;
  acc := !inner_acc + !acc;
  !acc

let () = Printf.printf "%d\n" second_part
