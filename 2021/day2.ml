let lines = In_channel.stdin |> In_channel.input_lines;;
let (final_horizontal, final_depth) = List.fold_left (fun (forward_amt, depth_amt) l ->
  let (command :: amt :: _) = String.split_on_char ' ' l in
  let amt_num = int_of_string amt in
  match command with
  | "forward" -> (forward_amt + amt_num, depth_amt)
  | "up" -> (forward_amt, depth_amt - amt_num)
  | "down" -> (forward_amt, depth_amt + amt_num)
  | _ -> failwith "Not possible"
) (0, 0) lines;;
Printf.printf "%d\n" (final_horizontal * final_depth);;

let (final_horizontal, final_depth, final_aim) = List.fold_left (fun (horizontal_amt, depth_amt, aim) l ->
  let (command :: amt :: _) = String.split_on_char ' ' l in
  let amt_num = int_of_string amt in
  match command with
  | "forward" -> (horizontal_amt + amt_num, depth_amt + aim * amt_num, aim)
  | "up" -> (horizontal_amt, depth_amt, aim - amt_num)
  | "down" -> (horizontal_amt, depth_amt, aim + amt_num)
  | _ -> failwith "Not possible"
) (0, 0, 0) lines;;
Printf.printf "%d\n" (final_horizontal * final_depth);;
