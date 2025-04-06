let lines = In_channel.input_lines (In_channel.stdin);;

let (_, ans) = List.fold_left (fun (prev, count) l ->
  let num = int_of_string l in
  if num > prev then
    (num, count + 1)
  else (num, count)) (List.hd lines |> int_of_string, 0) lines in
Printf.printf "%d\n" ans;;

let (_, ans) = List.fold_left (fun (lst, count) l ->
  let num = int_of_string l in
  match lst with
  | h::h2::h3::t -> (
    let sum = h + h2 + h3 in
    let new_lst = h2::h3::num::t in
    if h2 + h3 + num > sum then (new_lst, count + 1) else (new_lst, count)
  )
  | h::h2::t -> (h::h2::num::t, count)
  | h::t -> (h::num::t, count)
  | _ -> (num::lst, count)) ([], 0) lines in
Printf.printf "%d\n" ans;;
