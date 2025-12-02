let file_contents =
  In_channel.with_open_bin "day2.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let ranges =
  String.split_on_char ',' file_contents
  |> List.map (fun s ->
      String.split_on_char '-' s |> List.map (fun s -> int_of_string s))

let ranges =
  List.map
    (fun lst ->
      let arr = Array.of_list lst in
      (arr.(0), arr.(1)))
    ranges

let first_part =
  List.fold_left
    (fun acc (start, end') ->
      let invalid = ref acc in
      for i = start to end' do
        let s = string_of_int i in
        let len = String.length s in
        if
          len mod 2 = 0
          && String.sub s 0 (len / 2) = String.sub s (len / 2) (len / 2)
        then invalid := !invalid + i
      done;
      !invalid)
    0 ranges

let () = Printf.printf "%d\n" first_part

let chunk_string s chunk_length =
  let len = String.length s in
  let rec chunk' idx acc =
    if idx >= len then acc
    else
      chunk' (idx + chunk_length)
        (String.sub s idx (min (len - idx) chunk_length) :: acc)
  in
  chunk' 0 []

let check_invalid s =
  let rec loop chunk_length =
    if chunk_length = 0 then false
    else
      let chunks = chunk_string s chunk_length in
      if List.for_all (fun chunk -> chunk = List.hd chunks) chunks then true
      else loop (chunk_length - 1)
  in
  loop (String.length s / 2)

let second_part =
  List.fold_left
    (fun acc (start, end') ->
      let invalid = ref acc in
      for i = start to end' do
        let s = string_of_int i in
        if check_invalid s then invalid := !invalid + i
      done;
      !invalid)
    0 ranges

let () = Printf.printf "%d\n" second_part
