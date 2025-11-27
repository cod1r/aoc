type point = { x : int; y : int }

let print_point p =
  Printf.printf "point: %d, %d" p.x p.y;
  flush_all ()

let file_contents =
  In_channel.with_open_bin "day5.txt" (fun ic -> In_channel.input_all ic)

let lines =
  String.split_on_char '\n' file_contents |> List.filter (fun s -> s <> "")

let points =
  List.map
    (fun s ->
      String.split_on_char ' ' s
      |> List.filter (fun s -> s <> "->" && s <> "")
      |> List.map (fun s -> String.split_on_char ',' s |> Array.of_list)
      |> List.map (fun a ->
          { x = int_of_string a.(0); y = int_of_string a.(1) })
      |> Array.of_list)
    lines

let points_part1 =
  List.filter
    (fun arr ->
      let first_point = arr.(0) and second_point = arr.(1) in
      first_point.x = second_point.x || first_point.y = second_point.y)
    points

let is_point_on_segment point point_a point_b =
  if point_a.x = point_b.x && point.x = point_a.x then
    (point.y >= point_a.y && point.y <= point_b.y)
    || (point.y >= point_b.y && point.y <= point_a.y)
  else if point_a.y = point_b.y && point.y = point_a.y then
    (point.x >= point_a.x && point.x <= point_b.x)
    || (point.x >= point_b.x && point.x <= point_a.x)
  else
    point_a.x <> point_b.x
    &&
    let slope = (point_a.y - point_b.y) / (point_a.x - point_b.x) in
    let intercept = point_a.y - (slope * point_a.x) in
    point.y = (slope * point.x) + intercept

let get_overlapping_horizontal first_point second_point first_point'
    second_point' =
  let diff = second_point.x - first_point.x in
  let direction = diff / abs diff in
  let start = ref first_point.x in
  let counter = ref [] in
  while !start <> second_point.x + direction do
    let p = { x = !start; y = first_point.y } in
    if is_point_on_segment p first_point' second_point' then
      counter := p :: !counter;
    start := !start + direction
  done;
  !counter

let get_overlapping_vertical first_point second_point first_point' second_point'
    =
  let diff = second_point.y - first_point.y in
  let direction = diff / abs diff in
  let start = ref first_point.y in
  let counter = ref [] in
  while !start <> second_point.y + direction do
    let p = { x = first_point.x; y = !start } in
    if is_point_on_segment p first_point' second_point' then
      counter := p :: !counter;
    start := !start + direction
  done;
  !counter

let get_overlapping first_point second_point first_point' second_point' =
  let diff_y = second_point.y - first_point.y in
  let diff_x = second_point.x - first_point.x in
  let direction_y = if diff_y = 0 then 0 else diff_y / abs diff_y in
  let direction_x = if diff_x = 0 then 0 else diff_x / abs diff_x in
  let start = ref first_point in
  let counter = ref [] in
  while
    !start
    <> { x = second_point.x + direction_x; y = second_point.y + direction_y }
  do
    let p = { x = !start.x; y = !start.y } in
    if is_point_on_segment p first_point' second_point' then
      counter := p :: !counter;
    start := { x = !start.x + direction_x; y = !start.y + direction_y }
  done;
  !counter

let first_part =
  let unique = ref [] in
  let rec calc_first_part lst =
    match lst with
    | [] -> ()
    | h :: tl ->
        let first_point = h.(0) and second_point = h.(1) in
        List.iter
          (fun arr ->
            let first_point' = arr.(0) and second_point' = arr.(1) in
            let overlapping =
              get_overlapping first_point second_point first_point'
                second_point'
            in
            let unique_overlapping =
              List.filter
                (fun p -> not (List.exists (fun p' -> p' = p) !unique))
                overlapping
            in
            unique := unique_overlapping @ !unique)
          tl;
        calc_first_part tl
  in
  calc_first_part points_part1;
  List.length !unique

let () = Printf.printf "%d\n" first_part
(* 6482 is too high *)

(* let second_part =
  let unique = ref [] in
  let rec calc_first_part lst =
    match lst with
    | [] -> ()
    | h :: tl ->
        let first_point = h.(0) and second_point = h.(1) in
        List.iter
          (fun arr ->
            let first_point' = arr.(0) and second_point' = arr.(1) in
            let overlapping =
              get_overlapping first_point second_point first_point'
                second_point'
            in
            let unique_overlapping =
              List.filter
                (fun p -> not (List.exists (fun p' -> p' = p) !unique))
                overlapping
            in
            unique := unique_overlapping @ !unique)
          tl;
        calc_first_part tl
  in
  calc_first_part points;
  List.length !unique

let () = Printf.printf "%d\n" second_part *)
(* 15879 too low *)
(* 23504 too high *)

let array = Array.init 1000 (fun _ -> Array.init 1000 (fun _ -> 0))
let () =
  List.iter (fun arr ->
  let first_point = arr.(0)
  and second_point = arr.(1) in
  let diff_y = second_point.y - first_point.y in
  let diff_x = second_point.x - first_point.x in
  let direction_y = if diff_y = 0 then 0 else diff_y / abs diff_y in
  let direction_x = if diff_x = 0 then 0 else diff_x / abs diff_x in
  let start = ref first_point in
  while
    !start
    <> { x = second_point.x + direction_x; y = second_point.y + direction_y }
  do
    array.(!start.x).(!start.y) <- array.(!start.x).(!start.y) + 1;
    start := { x = !start.x + direction_x; y = !start.y + direction_y }
  done) points

let second_part = ref 0
let () =
for i = 0 to 999 do
  for j = 0 to 999 do
    if array.(i).(j) > 1 then second_part := !second_part + 1
  done
done

let () = Printf.printf "%d\n" !second_part
