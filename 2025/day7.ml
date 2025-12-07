let file_contents =
  In_channel.with_open_bin "day7.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let lines = String.split_on_char '\n' file_contents
let matrix = List.map (fun line -> Bytes.of_string line) lines |> Array.of_list

let splitters =
  let rec loop row_idx acc =
    if row_idx = Array.length matrix then acc
    else
      (let _, col_indices =
         Bytes.fold_left
           (fun (idx, acc') c ->
             if c = '^' then (idx + 1, idx :: acc') else (idx + 1, acc'))
           (0, []) matrix.(row_idx)
       in
       List.map (fun col -> (row_idx, col)) col_indices @ acc)
      |> loop (row_idx + 1)
  in
  loop 0 [] |> List.rev

let s_location =
  let row =
    Array.find_index (fun b -> Bytes.index_opt b 'S' |> Option.is_some) matrix
  in
  match row with
  | Some row ->
      let col = Bytes.index matrix.(row) 'S' in
      (row, col)
  | None -> failwith "impossible"

let first_part =
  let rec solve beam_positions splitters_used =
    if beam_positions = [] then splitters_used
    else
      let new_beam_positions, splitters_used =
        List.fold_left
          (fun (acc, acc2) (bpy, bpx) ->
            let underneath_splitter =
              List.find_opt
                (fun (splittery, splitterx) ->
                  bpx = splitterx && splittery > bpy)
                splitters
            in
            match underneath_splitter with
            | Some (underneath_splittery, underneath_splitterx) ->
                let left = (underneath_splittery, underneath_splitterx - 1)
                and right = (underneath_splittery, underneath_splitterx + 1) in
                ( left :: right
                  :: List.filter (fun bp -> bp <> left && bp <> right) acc,
                  (underneath_splittery, underneath_splitterx) :: acc2 )
            | None -> (acc, acc2))
          ([], splitters_used) beam_positions
      in
      solve new_beam_positions splitters_used
  in
  let splitters_used = solve [ s_location ] [] in
  let unique =
    List.fold_left
      (fun acc sp ->
        if List.exists (fun sp' -> sp' = sp) acc then acc else sp :: acc)
      [] splitters_used
  in
  List.length unique

let () = Printf.printf "%d\n" first_part

let second_part =
  let memo =
    ref
      (Array.init_matrix
         (Bytes.length matrix.(0))
         (Array.length matrix)
         (fun _ _ -> None))
  in
  let rec time_split tachyon_particle_pos =
    let y, x = tachyon_particle_pos in
    let splitter =
      List.find_opt (fun (spy, spx) -> spy > y && x = spx) splitters
    in
    match splitter with
    | Some (spy, spx) ->
        if Option.is_some !memo.(spy).(spx) then Option.get !memo.(spy).(spx)
        else
          let res = time_split (spy, spx - 1) + time_split (spy, spx + 1) in
          !memo.(spy).(spx) <- Some res;
          res
    | None -> 1
  in
  time_split s_location

let () = Printf.printf "%d\n" second_part
