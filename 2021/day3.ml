let lines = In_channel.stdin |> In_channel.input_lines;;
let bit_length = (List.hd lines |> String.length);;

let frequency bit_idx lst = List.fold_left (fun (one_cnt, zero_cnt) l ->
  match l.[bit_idx] with
  | '0' -> (one_cnt, zero_cnt + 1)
  | '1' -> (one_cnt + 1, zero_cnt)
  | _ -> failwith "Not possible") (0, 0) lst
;;

let gamma = ref 0 in
let epsilon = ref 0 in
for bit_idx = 0 to bit_length - 1 do
  let (one_cnt, zero_cnt) = frequency bit_idx lines in
  let (most_common, least_common) = if one_cnt > zero_cnt then 1, 0 else 0, 1 in
  gamma := !gamma + most_common * (1 lsl (bit_length - 1 - bit_idx));
  epsilon := !epsilon + least_common * (1 lsl (bit_length - 1 - bit_idx));
done;
Printf.printf "%d\n" (!gamma * !epsilon);;

let rec part2_most_common bit_idx lst =
  let (one_cnt, zero_cnt) = frequency bit_idx lst in
  let (most_common, _) = if one_cnt > zero_cnt then 1, 0 else 0, 1 in
  let keep_filter_value = if one_cnt = zero_cnt then 1 else most_common in
  let filtered_most_common = List.filter (fun l -> Char.code l.[bit_idx] - Char.code '0' = keep_filter_value) lst in
  if List.length filtered_most_common > 1 then
    part2_most_common (bit_idx + 1) filtered_most_common
  else
    List.hd filtered_most_common
;;

let parse_bit_string str =
  let num = ref 0 in
  let len = String.length str in
  String.iteri (fun idx char -> if char = '1' then num := !num + (1 lsl (len - 1 - idx))) str;
  !num
;;

let oxy_rating = part2_most_common 0 lines |> parse_bit_string;;

let rec part2_least_common bit_idx lst =
  let (one_cnt, zero_cnt) = frequency bit_idx lst in
  let (_, least_common) = if one_cnt > zero_cnt then 1, 0 else 0, 1 in
  let keep_filter_value = if one_cnt = zero_cnt then 0 else least_common in
  let filtered_least_common = List.filter (fun l -> Char.code l.[bit_idx] - Char.code '0' = keep_filter_value) lst in
  if List.length filtered_least_common > 1 then
    part2_least_common (bit_idx + 1) filtered_least_common
  else
    List.hd filtered_least_common
;;

let co2_rating = part2_least_common 0 lines |> parse_bit_string;;

Printf.printf "%d\n" (oxy_rating * co2_rating);;
