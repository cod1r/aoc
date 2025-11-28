let file_contents =
  In_channel.with_open_bin "day7.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let horizontal_positions =
  String.split_on_char ',' file_contents
  |> List.map (fun s -> int_of_string s)
  |> Array.of_list

let () = Array.sort (fun a b -> a - b) horizontal_positions

let first_part =
  let min_cost = ref Int.max_int in
  let low = horizontal_positions.(0)
  and high = horizontal_positions.(Array.length horizontal_positions - 1) in
  for i = low to high do
    let cost =
      Array.fold_left (fun acc v -> acc + abs (v - i)) 0 horizontal_positions
    in
    min_cost := min !min_cost cost
  done;
  !min_cost

let () = Printf.printf "%d\n" first_part

let second_part =
  let min_cost = ref Int.max_int in
  let low = horizontal_positions.(0)
  and high = horizontal_positions.(Array.length horizontal_positions - 1) in
  for i = low to high do
    let cost =
      Array.fold_left
        (fun acc v ->
          acc
          +
          let a = abs (v - i) in
          a * (a + 1) / 2)
        0 horizontal_positions
    in
    min_cost := min !min_cost cost
  done;
  !min_cost

let () = Printf.printf "%d\n" second_part
