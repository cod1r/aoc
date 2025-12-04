let file_contents =
  In_channel.with_open_bin "day4.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let matrix =
  String.split_on_char '\n' file_contents
  |> List.map (fun line -> String.to_bytes line)
  |> Array.of_list

let accessible i j =
  let positions =
    [|
      (i - 1, j - 1);
      (i - 1, j);
      (i - 1, j + 1);
      (i, j - 1);
      (i, j + 1);
      (i + 1, j - 1);
      (i + 1, j);
      (i + 1, j + 1);
    |]
  in
  let rolls_of_paper =
    Array.fold_left
      (fun acc (y, x) ->
        if
          y < 0 || x < 0
          || y = Array.length matrix
          || x = Bytes.length matrix.(y)
        then acc
        else
          match Bytes.get matrix.(y) x = '@' with
          | true -> acc + 1
          | false -> acc)
      0 positions
  in
  rolls_of_paper < 4

let first_part =
  let ans = ref 0 in
  for i = 0 to Array.length matrix - 1 do
    for j = 0 to Bytes.length matrix.(i) - 1 do
      if Bytes.get matrix.(i) j = '@' then
        if accessible i j then ans := !ans + 1
    done
  done;
  !ans

let () = Printf.printf "%d\n" first_part

(* 11848 rolls in the matrix for our input *)
let second_part =
  let rec removal acc =
    let ans = ref 0 in
    for i = 0 to Array.length matrix - 1 do
      for j = 0 to Bytes.length matrix.(i) - 1 do
        if Bytes.get matrix.(i) j = '@' then
          if accessible i j then (
            Bytes.set matrix.(i) j '.';
            ans := !ans + 1)
      done
    done;
    if !ans = 0 then !ans + acc else removal (!ans + acc)
  in
  removal 0

let () = Printf.printf "%d\n" second_part
