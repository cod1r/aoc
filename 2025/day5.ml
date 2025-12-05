let file_contents =
  In_channel.with_open_bin "day5.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let lines = String.split_on_char '\n' file_contents

let fresh_ranges =
  lines
  |> List.take_while (fun line -> line <> "")
  |> List.map (fun line ->
      let lst = String.split_on_char '-' line in
      let numbers = List.map (fun n -> int_of_string n) lst in
      Array.of_list numbers)

let available_ids =
  lines
  |> List.drop_while (fun line -> line <> "")
  |> List.drop 1
  |> List.map (fun n -> int_of_string n)

let first_part =
  List.fold_left
    (fun acc id ->
      if List.exists (fun arr -> arr.(0) <= id && arr.(1) >= id) fresh_ranges
      then acc + 1
      else acc)
    0 available_ids

let () = Printf.printf "%d\n" first_part

let rec combine_ranges combined =
  let combined' =
    List.fold_left
      (fun acc arr ->
        let overlapping_rng =
          List.find_opt
            (fun rng -> rng.(0) <= arr.(1) && arr.(0) <= rng.(1))
            acc
        in
        match overlapping_rng with
        | Some overlapping_rng ->
            [|
              min overlapping_rng.(0) arr.(0); max overlapping_rng.(1) arr.(1);
            |]
            :: List.filter (fun rng -> rng <> overlapping_rng) acc
        | None -> arr :: acc)
      [] combined
  in
  if List.length combined != List.length combined' then combine_ranges combined'
  else combined

let combined_ranges = combine_ranges fresh_ranges

(* let () =
  List.iter
    (fun range -> Printf.printf "%d %d\n" range.(0) range.(1))
    combined_ranges *)

let second_part =
  List.fold_left
    (fun acc range -> acc + range.(1) - range.(0) + 1)
    0 combined_ranges

let () = Printf.printf "%d\n" second_part

(* 321713401631775 too low *)
