let file, pair_count = ("day8.txt", 1000)

let file_contents =
  In_channel.with_open_bin file (fun ic -> In_channel.input_all ic)
  |> String.trim

type three_dim_point = { x : int; y : int; z : int }

let print_point p = Printf.printf "%d %d %d\n" p.x p.y p.z

let string_of_point p =
  Int.to_string p.x ^ "," ^ Int.to_string p.y ^ "," ^ Int.to_string p.z

let lines = String.split_on_char '\n' file_contents

let points =
  List.map
    (fun line ->
      let parts =
        String.split_on_char ',' line
        |> List.map (fun p -> int_of_string p)
        |> Array.of_list
      in
      { x = parts.(0); y = parts.(1); z = parts.(2) })
    lines

let dist p1 p2 =
  let diff_x = p1.x - p2.x and diff_y = p1.y - p2.y and diff_z = p1.z - p2.z in
  (diff_x * diff_x) + (diff_y * diff_y) + (diff_z * diff_z)

let pairs =
  let rec fold acc points =
    match points with
    | [] -> acc
    | point :: tl ->
        let acc = acc @ List.map (fun p -> (point, p)) tl in
        fold acc tl
  in
  fold [] points

let sorted =
  List.sort
    (fun pair1 pair2 ->
      dist (fst pair1) (snd pair1) - dist (fst pair2) (snd pair2))
    pairs

(* let () =
  List.iter
    (fun (p1, p2) ->
      print_point p1;
      print_point p2;
      Printf.printf "%d\n" (dist p1 p2) )
    sorted *)

let sorted_for_part1 = List.take pair_count sorted

module Circuit = struct
  type t = three_dim_point

  let compare a b =
    let first, second, third =
      (Int.compare a.x b.x, Int.compare a.y b.y, Int.compare a.z b.z)
    in
    if first <> 0 then first else if second <> 0 then second else third
end

module CircuitSet = Set.Make (Circuit)

let circuits =
  List.fold_left
    (fun circuits (junction_box1, junction_box2) ->
      let circuits_not_disjoint =
        List.find_all
          (fun circuit ->
            CircuitSet.mem junction_box1 circuit
            || CircuitSet.mem junction_box2 circuit)
          circuits
      in
      let circuits =
        List.filter
          (fun circuit ->
            not
              (List.exists
                 (fun circuit' -> circuit' == circuit)
                 circuits_not_disjoint))
          circuits
      in
      let starting_circuit =
        CircuitSet.add junction_box1 CircuitSet.empty
        |> CircuitSet.add junction_box2
      in
      let merged_circuit =
        List.fold_left
          (fun acc c -> CircuitSet.union acc c)
          starting_circuit circuits_not_disjoint
      in
      merged_circuit :: circuits)
    [] sorted_for_part1

let decreasing_circuit_size =
  List.sort (fun a b -> CircuitSet.cardinal b - CircuitSet.cardinal a) circuits

(* let () = Printf.printf "%d\n" (List.length circuits) *)

(* let () =
  List.iter
    (fun circuit ->
      List.iter
        (fun circuit' ->
          if circuit <> circuit' && not (CircuitSet.disjoint circuit circuit')
          then failwith "WHAT")
        circuits)
    circuits *)

let first_part =
  List.fold_left
    (fun acc circuit -> acc * CircuitSet.cardinal circuit)
    1
    (List.take 3 decreasing_circuit_size)

let () = Printf.printf "%d\n" first_part
(* 20925 too low *)
(* 9200 too low *)

let part2 =
  let rec fold circuits pairs junction_boxes =
    match pairs with
    | [] -> 0
    | (junction_box1, junction_box2) :: tl ->
        let filtered_junction_boxes =
          List.filter
            (fun jb -> jb <> junction_box1 && jb <> junction_box2)
            junction_boxes
        in
        (* part2 is confusing but it's basically asking for the last
        pair that get connected to cause the amount of circuits that contain only one junction box to be 0 *)
        if
          List.length filtered_junction_boxes = 0
          && List.length junction_boxes = 1
        then
          let { x; _ } = junction_box1 and { x = x'; _ } = junction_box2 in
          x * x'
        else
          let has_jb1 =
            List.exists (fun jb -> jb = junction_box1) junction_boxes
          and has_jb2 =
            List.exists (fun jb -> jb = junction_box2) junction_boxes
          in
          let new_circuits =
            if has_jb1 && has_jb2 then
              (CircuitSet.empty
              |> CircuitSet.add junction_box1
              |> CircuitSet.add junction_box2)
              :: circuits
            else if (not has_jb1) && has_jb2 then
              List.map
                (fun circuit ->
                  if CircuitSet.mem junction_box1 circuit then
                    CircuitSet.add junction_box2 circuit
                  else circuit)
                circuits
            else if has_jb1 && not has_jb2 then
              List.map
                (fun circuit ->
                  if CircuitSet.mem junction_box2 circuit then
                    CircuitSet.add junction_box1 circuit
                  else circuit)
                circuits
            else
              let has_jb1 =
                List.find
                  (fun circuit -> CircuitSet.mem junction_box1 circuit)
                  circuits
              and has_jb2 =
                List.find
                  (fun circuit -> CircuitSet.mem junction_box2 circuit)
                  circuits
              in
              let merged = CircuitSet.union has_jb1 has_jb2 in
              merged
              :: List.filter
                   (fun circuit -> circuit <> has_jb1 && circuit <> has_jb2)
                   circuits
          in
          fold new_circuits tl filtered_junction_boxes
  in
  fold [] sorted points

let () = Printf.printf "%d\n" part2

(* 73276500 too low *)
