let file_contents =
  In_channel.with_open_bin "day1.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

type turn = L | R
type rotation_info = { turn : turn; amt : int }

let rotations =
  String.split_on_char '\n' file_contents
  |> List.map (fun s ->
      let t =
        match String.sub s 0 1 with
        | "L" -> L
        | "R" -> R
        | _ -> failwith "IMPOSSIBLE"
      in
      { turn = t; amt = String.sub s 1 (String.length s - 1) |> int_of_string })

let rec part1 pointing_at rotations acc =
  let acc = if pointing_at = 0 then acc + 1 else acc in
  match rotations with
  | [] -> acc
  | h :: tl ->
      let { turn; amt } = h in
      let new_pointing_at =
        match turn with
        | L -> pointing_at - (amt mod 100)
        | R -> pointing_at + (amt mod 100)
      in
      let new_pointing_at =
        if new_pointing_at < 0 then 100 + new_pointing_at
        else if new_pointing_at > 99 then new_pointing_at mod 100
        else new_pointing_at
      in
      part1 new_pointing_at tl acc

let first_part = part1 50 rotations 0
let () = Printf.printf "%d\n" first_part
(* 728 too low *)

let rec part2 pointing_at rotations acc =
  let acc = if pointing_at = 0 then acc + 1 else acc in
  match rotations with
  | [] -> acc
  | h :: tl ->
      let { turn; amt } = h in
      let new_pointing_at, added =
        match turn with
        | L ->
            ( (100 + pointing_at - (amt mod 100)) mod 100,
              (amt / 100)
              +
              if pointing_at - (amt mod 100) < 0 && pointing_at <> 0 then 1
              else 0 )
        | R ->
            ( (pointing_at + (amt mod 100)) mod 100,
              (amt / 100)
              +
              if (amt mod 100) + pointing_at > 100 && pointing_at <> 0 then 1
              else 0 )
      in
      part2 new_pointing_at tl (acc + added)

let second_part = part2 50 rotations 0
let () = Printf.printf "%d\n" second_part
(* 6364 too high *)
