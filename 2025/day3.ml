let file_contents =
  In_channel.with_open_bin "day3.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let banks_of_batteries = String.split_on_char '\n' file_contents

let first_part =
  let get_bank_joltage s =
    let rec loop idx acc =
      let sub = String.sub s (idx + 1) (String.length s - (idx + 1)) in
      let acc =
        String.fold_left
          (fun acc c ->
            let str = String.make 1 s.[idx] ^ String.make 1 c in
            max acc (int_of_string str))
          acc sub
      in
      if idx = String.length s - 1 then acc else loop (idx + 1) acc
    in
    loop 0 0
  in
  List.fold_left (fun acc s -> acc + get_bank_joltage s) 0 banks_of_batteries

let () = Printf.printf "%d\n" first_part

let second_part =
  let get_range s start len_of_str =
    let end' = String.length s - (12 - len_of_str) in
    (start, end')
  in
  let get_bank_joltage s =
    let bytes = Bytes.init 12 (fun _ -> '0') in
    let get_largest_battery_jolt str =
      String.fold_left
        (fun acc c ->
          let ascii0 = Char.code '0' in
          if Char.code c - ascii0 > Char.code acc - ascii0 then c else acc)
        '0' str
    in
    let rec calc_bank_jolt idx (start, end') =
      if idx = 12 then bytes
      else
        let sub = String.sub s start (end' - start + 1) in
        let largest = get_largest_battery_jolt sub in
        Bytes.set bytes idx largest;
        let idx_of = String.index sub largest in
        let new_range = get_range s (start + idx_of + 1) (idx + 1) in
        calc_bank_jolt (idx + 1) new_range
    in
    calc_bank_jolt 0 (get_range s 0 0) |> String.of_bytes |> int_of_string
  in
  List.fold_left (fun acc s -> acc + get_bank_joltage s) 0 banks_of_batteries

let () = Printf.printf "%d\n" second_part
