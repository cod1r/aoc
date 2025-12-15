let file_contents =
  In_channel.with_open_bin "day12.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let lines = String.split_on_char '\n' file_contents

let shapes, area_requirements =
  lines
  |> List.fold_left
       (fun (shapes, acc) line ->
         if line = "" then (List.rev acc :: shapes, [])
         else (shapes, line :: acc))
       ([], [])
  |> fun (shapes, area_requirements) ->
  (List.rev shapes, List.rev area_requirements)

let shapes =
  shapes
  |> List.map (fun shape ->
      let shape = List.drop 1 shape in
      let shape = List.map (fun s -> Bytes.of_string s) shape in
      let shape = Array.of_list shape in
      shape)
  |> Array.of_list

type requirements = { width : int; height : int; shape_amts : int array }

let area_requirements =
  area_requirements
  |> List.map (fun requirements ->
      let parts = String.split_on_char ' ' requirements in
      let area =
        List.hd parts
        |> (fun s -> String.sub s 0 (String.length s - 1))
        |> String.split_on_char 'x' |> List.map int_of_string |> Array.of_list
      in
      let shape_amts =
        List.drop 1 parts |> List.map int_of_string |> Array.of_list
      in
      { width = area.(0); height = area.(1); shape_amts })
  |> List.sort (fun a b -> (a.height * a.width) - (b.height * b.width))

let mirror_shape shape (direction : [ `Horizontal | `Vertical ]) =
  let shape = Array.map (fun b -> Bytes.copy b) shape in
  begin match direction with
  | `Vertical ->
      for idx = 0 to Array.length shape / 2 do
        let temp = shape.(idx) in
        let flip_idx = Array.length shape - idx - 1 in
        shape.(idx) <- shape.(flip_idx);
        shape.(flip_idx) <- temp
      done
  | `Horizontal ->
      Array.iter
        (fun b ->
          for i = 0 to Bytes.length b / 2 do
            let temp = Bytes.get b i in
            let flip_idx = Bytes.length b - i - 1 in
            let other = Bytes.get b flip_idx in
            Bytes.set b i other;
            Bytes.set b flip_idx temp
          done)
        shape
  end;
  shape

(* you can transpose a square and then mirror it vertically to get a 90 degree cw rotation *)
let rotate_cw_90_degrees shape =
  let shape = Array.map (fun b -> Bytes.copy b) shape in
  let arr_len = Array.length shape in
  for i = 0 to arr_len - 1 do
    for j = 0 to arr_len - i - 1 do
      let temp = Bytes.get shape.(i) j in
      let other = Bytes.get shape.(arr_len - j - 1) (arr_len - i - 1) in
      Bytes.set shape.(i) j other;
      Bytes.set shape.(arr_len - j - 1) (arr_len - i - 1) temp
    done
  done;
  mirror_shape shape `Vertical

let print_shape shape =
  Array.iter
    (fun b ->
      Bytes.iter (fun c -> print_char c) b;
      print_newline ())
    shape

module PointsSetSig = struct
  type t = int * int

  let compare a b =
    let first = Int.compare (fst a) (fst b) in
    if first = 0 then Int.compare (snd a) (snd b) else first
end

module PointsSet = Set.Make (PointsSetSig)

let get_all_rotations shape =
  let rotations = ref [] in
  let curr = ref shape in
  for _ = 0 to 3 do
    let rotated = rotate_cw_90_degrees !curr in
    rotations := rotated :: !rotations;
    curr := rotated
  done;
  !rotations

let get_points_for_shape shape =
  let points = ref [] in
  for i = 0 to 2 do
    for j = 0 to 2 do
      if Bytes.get shape.(i) j = '#' then points := (i, j) :: !points
    done
  done;
  !points

let get_shape_area shape =
  let area = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      if Bytes.get shape.(i) j = '#' then area := !area + 1
    done
  done;
  !area

let area_requirements =
  area_requirements
  |> List.filter (fun requirement ->
      let _, sum =
        Array.fold_left
          (fun (idx, acc) shape_amt ->
            (idx + 1, acc + (get_shape_area shapes.(idx) * shape_amt)))
          (0, 0) requirement.shape_amts
      in
      requirement.width * requirement.height >= sum)

let rec can_place_one_of_each width height set_points lst =
  (* let matrix = Array.make_matrix height width '.' in
  PointsSet.iter (fun (y, x) -> matrix.(y).(x) <- '#') set_points;
  Array.iter
    (fun arr ->
      Array.iter (fun c -> print_char c) arr;
      print_newline ())
    matrix;
  print_newline (); *)
  match lst with
  | [] -> true
  | shape_options :: tl ->
      let placed = ref false in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          if not (PointsSet.mem (y, x) set_points) then
            let fitted =
              List.filter
                (fun shape ->
                  let can_fit = ref true in
                  for i = 0 to 2 do
                    for j = 0 to 2 do
                      if Bytes.get shape.(i) j = '#' then
                        can_fit :=
                          !can_fit
                          && i + y < height
                          && j + x < width
                          && not (PointsSet.mem (i + y, j + x) set_points)
                    done
                  done;
                  !can_fit)
                shape_options
            in
            placed :=
              !placed
              || List.exists
                   (fun fitted ->
                     let shape_points =
                       get_points_for_shape fitted
                       |> List.map (fun (y', x') -> (y + y', x + x'))
                     in
                     let set_points =
                       List.fold_left
                         (fun acc p -> PointsSet.add p acc)
                         set_points shape_points
                     in
                     can_place_one_of_each width height set_points tl)
                   fitted
        done
      done;
      !placed

let can_fit_all_shapes requirement =
  let rec place_shapes idx acc =
    if idx = Array.length requirement.shape_amts then acc
    else
      let amt = requirement.shape_amts.(idx) in
      let shape = shapes.(idx) in
      let mirrored = mirror_shape shape `Vertical in
      let shape_versions =
        get_all_rotations shape @ get_all_rotations mirrored
      in
      place_shapes (idx + 1)
        (if amt > 0 then List.init amt (fun _ -> shape_versions) @ acc else acc)
  in
  let list_of_shape_options = place_shapes 0 [] in
  can_place_one_of_each requirement.width requirement.height PointsSet.empty
    list_of_shape_options

let () = Printf.printf "%d\n" (List.length area_requirements)

let () =
  List.iter
    (fun requirement ->
      Printf.printf "%d %d " requirement.width requirement.height;
      Array.iter (fun amt -> Printf.printf "%d " amt) requirement.shape_amts;
      let _, sum =
        Array.fold_left
          (fun (idx, acc) shape_amt ->
            (idx + 1, acc + (get_shape_area shapes.(idx) * shape_amt)))
          (0, 0) requirement.shape_amts
      in
      Printf.printf "%d %b" sum (requirement.width * requirement.height < sum);
      print_newline ())
    area_requirements

let first_part =
  area_requirements
  |> List.fold_left
       (fun acc requirement ->
         if can_fit_all_shapes requirement then (acc + 1) else acc)
       0

let () = Printf.printf "%d\n" first_part
(*
the brute force I wrote above works but is too slow to determine if
some area cannot contain the shapes required to fit.

ideas:
i could cache some computed answers that have the subset of shapes required to fit in some area and then use that to solve others but that doesn't necessarily help with required shapes that involve some different combination.

the only way to truly know if some set of shapes cannot be fit together,
is to get the most compact fitting for a subset of the shapes and then try
to fit again and failing.
*)
