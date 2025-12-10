let file_contents =
  In_channel.with_open_bin "day9.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let red_points =
  String.split_on_char '\n' file_contents
  |> List.map (fun line ->
      let parts = String.split_on_char ',' line in
      let numbers = List.map (fun p -> int_of_string p) parts in
      let arr = Array.of_list numbers in
      (arr.(0), arr.(1)))

let get_area p1 p2 = (abs (fst p1 - fst p2) + 1) * (abs (snd p1 - snd p2) + 1)
let print_point p = Printf.printf "%d %d\n" (fst p) (snd p)

let pairs =
  let rec fold acc pairs =
    match pairs with
    | [] -> acc
    | p :: tl -> fold (acc @ List.map (fun p' -> (p, p')) tl) tl
  in
  fold [] red_points

let first_part =
  let sorted =
    List.sort
      (fun (p1, p2) (p1', p2') -> get_area p1' p2' - get_area p1 p2)
      pairs
  in
  let p1, p2 = List.hd sorted in
  get_area p1 p2

let () = Printf.printf "%d\n" first_part

(* let smallest_x =
  List.fold_left (fun acc p -> min acc (fst p)) Int.max_int red_points

let largest_x =
  List.fold_left (fun acc p -> max acc (fst p)) Int.min_int red_points

let smallest_y =
  List.fold_left (fun acc p -> min acc (snd p)) Int.max_int red_points

let largest_y =
  List.fold_left (fun acc p -> max acc (snd p)) Int.min_int red_points

let () =
  Printf.printf "%d %d %d %d" smallest_x smallest_y largest_x largest_y;
  print_newline ()

let () =
  Printf.printf "%d\n"
    (get_area (smallest_x, smallest_y) (largest_x, largest_y));
  print_newline () *)

let _, red_points =
  List.fold_left
    (fun (start, acc) p -> (p, (start, p) :: acc))
    (List.hd red_points, [])
    (List.drop 1 red_points @ [ List.hd red_points ])

let vertical =
  List.filter
    (fun (p1, p2) ->
      let diff_x = Int.compare (fst p2) (fst p1) in
      diff_x = 0)
    red_points
  |> List.sort (fun (p1, _) (p2, _) -> fst p1 - fst p2)

let second_part =
  let valid =
    List.filter
      (fun (p1, p2) ->
        let min_x = min (fst p1) (fst p2)
        and max_x = max (fst p1) (fst p2)
        and min_y = min (snd p1) (snd p2)
        and max_y = max (snd p1) (snd p2) in
        let vertical_segments_before =
          List.filter
            (fun (p1, p2) ->
              let p1, p2 = if snd p1 > snd p2 then (p2, p1) else (p1, p2) in
              fst p1 < min_x + 1 && min_y + 1 >= snd p1 && min_y + 1 <= snd p2)
            vertical
        in
        let res =
          not
            (List.exists
               (fun (start, end') ->
                 let diff_x = Int.compare (fst end') (fst start)
                 and diff_y = Int.compare (snd end') (snd start) in
                 let between =
                   if diff_x = 0 then
                     let start, end' =
                       if snd start > snd end' then (end', start)
                       else (start, end')
                     in
                     fst start > min_x
                     && fst start < max_x
                     && snd start < max_y
                     && min_y < snd end'
                   else if diff_y = 0 then
                     let start, end' =
                       if fst start > fst end' then (end', start)
                       else (start, end')
                     in
                     snd start > min_y
                     && snd start < max_y
                     && fst start < max_x
                     && min_x < fst end'
                   else failwith "IMPOSSIBLE"
                 in
                 start <> p1 && start <> p2 && end' <> p1 && end' <> p2
                 && between)
               red_points)
        in
        res && List.length vertical_segments_before mod 2 <> 0)
      pairs
  in
  List.fold_left (fun acc (p1, p2) -> max acc (get_area p1 p2)) 0 valid

let () = Printf.printf "%d\n" second_part
(* 4549723596 too high *)
(* 4600181596 *)
(* 4508605978 too high *)
