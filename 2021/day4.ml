let file_contents =
  In_channel.with_open_bin "day4_part1.txt" (fun ic -> In_channel.input_all ic)

let numbers_to_be_drawn, boards_text =
  String.split_on_char '\n' file_contents |> function
  | [] -> ([], [])
  | h :: tl ->
      ( String.split_on_char ',' h |> List.map (fun s -> int_of_string s),
        List.filter (fun s -> s <> "") tl )

let get_board_and_rest s_list = (List.take 5 s_list, List.drop 5 s_list)

let boards =
  let rec get_boards boards_text acc =
    match boards_text with
    | [] -> acc
    | _ ->
        let board, boards_text = get_board_and_rest boards_text in
        get_boards boards_text (board :: acc)
  in
  get_boards boards_text []

let convert_board_to_matrix board =
  List.map
    (fun s ->
      String.split_on_char ' ' s
      |> List.filter (fun s -> s <> "")
      |> List.map (fun s -> int_of_string s)
      |> Array.of_list)
    board
  |> Array.of_list

let boards = List.map (fun board -> convert_board_to_matrix board) boards

type annotation = Marked of int | Unmarked of int

let annotate_board board =
  Array.map (fun arr -> Array.map (fun n -> Unmarked n) arr) board

let boards = List.map (fun board -> annotate_board board) boards

let mark_number n board =
  Array.map
    (fun arr ->
      Array.map
        (function
          | Unmarked n' as unmarked -> if n' = n then Marked n else unmarked
          | Marked _ as marked -> marked)
        arr)
    board

let check_board board =
  Array.fold_left
    (fun acc arr ->
      acc
      || Array.for_all (function Marked n -> true | Unmarked _ -> false) arr)
    false board
  || Array.fold_left
       (fun acc col ->
         let rows = Array.init (Array.length board.(0)) (fun c -> c) in
         acc
         || Array.for_all
              (fun row ->
                match board.(row).(col) with
                | Marked _ -> true
                | Unmarked _ -> false)
              rows)
       false
       (Array.init (Array.length board) (fun n -> n))

let sum_of_all_unmarked board =
  Array.fold_left
    (fun acc arr ->
      Array.fold_left
        (fun acc ann ->
          match ann with Unmarked n -> acc + n | Marked _ -> acc)
        acc arr)
    0 board

let print_board b =
  Array.iter
    (fun arr ->
      Array.iter
        (fun ann ->
          match ann with
          | Unmarked n -> Printf.printf "Unmarked: %d " n
          | Marked n -> Printf.printf "Marked: %d " n)
        arr;
      print_newline ())
    b

let rec go_until_winner lst boards =
  match lst with
  | [] -> 0
  | h :: tl -> (
      let boards = List.map (fun b -> mark_number h b) boards in
      let opt =
        List.find_map
          (fun board ->
            if check_board board then
              sum_of_all_unmarked board |> fun n -> Some (n * h)
            else None)
          boards
      in
      match opt with Some a -> a | None -> go_until_winner tl boards)

let rec find_last_winner lst boards =
  match lst with
  | [] -> 0
  | h :: tl ->
      let boards = List.map (fun b -> mark_number h b) boards in
      let len = List.length boards in
      if len = 1 && check_board (List.hd boards) then
        sum_of_all_unmarked (List.hd boards) |> fun n -> n * h
      else
        let boards = List.filter (fun b -> not (check_board b)) boards in
        find_last_winner tl boards

let first_part_final_score = go_until_winner numbers_to_be_drawn boards
let second_part_final_score = find_last_winner numbers_to_be_drawn boards
let () = Printf.printf "%d\n" first_part_final_score
let () = Printf.printf "%d\n" second_part_final_score
