let file_contents =
  In_channel.with_open_bin "day6.txt" (fun ic -> In_channel.input_all ic)

let initial_state =
  String.split_on_char ',' file_contents
  |> List.map (fun s -> String.trim s |> int_of_string)

let rec iterate day limit lst =
  if day = limit + 1 then lst
  else
    let lst =
      List.fold_left
        (fun acc n -> if n = 0 then 6 :: 8 :: acc else (n - 1) :: acc)
        [] lst
    in
    iterate (day + 1) limit lst

let first_part = iterate 0 79 initial_state

(* let () = List.iter (fun n -> Printf.printf "%d\n" n) first_part *)
let () = Printf.printf "%d\n\n" (List.length first_part)

module CountMap = struct
  type t = int

  let compare a b = Int.compare a b
end

module CountMap = Map.Make (CountMap)

let increment_count increment_amt =
 fun c -> increment_amt + Option.value c ~default:0 |> Option.some

let map =
  List.fold_left
    (fun acc n -> CountMap.update n (increment_count 1) acc)
    CountMap.empty initial_state

(* let () =
  Printf.printf "Initial map:\n";
  CountMap.iter (fun k v -> Printf.printf "%d %d\n" k v) map;
  print_newline () *)

let iterate days initial_map =
  let rec iterate' day limit map =
    if day = limit then map
    else
      let map =
        CountMap.fold
          (fun k v acc ->
            let increment_fn = increment_count v in
            if k = 0 then
              CountMap.update 6 increment_fn acc
              |> CountMap.update 8 increment_fn
            else CountMap.update (k - 1) increment_fn acc)
          map CountMap.empty
      in
      iterate' (day + 1) limit map
  in
  iterate' 0 days initial_map

let second_part_map = iterate 256 map
(* let () = CountMap.iter (fun k v -> Printf.printf "%d %d\n" k v) second_part_map *)
let second_part = CountMap.fold (fun k v acc -> v + acc) second_part_map 0
let () = Printf.printf "%d\n" second_part
