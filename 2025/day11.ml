let file_contents =
  In_channel.with_open_bin "day11_test.txt" (fun ic -> In_channel.input_all ic)
  |> String.trim

let lines = String.split_on_char '\n' file_contents |> Array.of_list

let mappings =
  Array.init (Array.length lines) (fun i ->
      let s = lines.(i) in
      let parts = String.split_on_char ' ' s in
      Array.of_list parts)

let () = Array.sort (fun a b -> String.compare a.(0) b.(0)) mappings

let binary_search arr value =
  let rec search left right =
    if left > right then None
    else
      let mid = (left + right) / 2 in
      if arr.(mid).(0) = value then Some arr.(mid)
      else if arr.(mid).(0) < value then search (mid + 1) right
      else search left (mid - 1)
  in
  search 0 (Array.length arr - 1)

let first_part =
  let rec dfs curr =
    if curr = "out:" then 1
    else
      let outs = binary_search mappings curr in
      match outs with
      | Some outs ->
          let sum = ref 0 in
          for i = 1 to Array.length outs - 1 do
            sum := !sum + dfs (outs.(i) ^ ":")
          done;
          !sum
      | None -> 0
  in
  dfs "you:"

let () = Printf.printf "%d\n" first_part

module SetCanReachOutSig = struct
  type t = string

  let compare = String.compare
end

module SetCanReachOut = Set.Make (SetCanReachOutSig)
module MemoMap = Map.Make (SetCanReachOutSig)

let dfs curr target =
  let memo = ref MemoMap.empty in
  let rec dfs' curr =
    if MemoMap.mem curr !memo then MemoMap.find curr !memo
    else if curr = target then 1
    else
      let outs = binary_search mappings curr in
      match outs with
      | Some outs ->
          let sum = ref 0 in
          for i = 1 to Array.length outs - 1 do
            sum := !sum + dfs' (outs.(i) ^ ":")
          done;
          memo := MemoMap.add curr !sum !memo;
          !sum
      | None -> 0
  in
  dfs' curr

let fft_to_dac = dfs "fft:" "dac:"
let dac_to_fft = dfs "dac:" "fft:"
let svr_to_fft = dfs "svr:" "fft:"
let svr_to_dac = dfs "svr:" "dac:"
let dac_to_out = dfs "dac:" "out:"
let fft_to_out = dfs "fft:" "out:"

let second_part =
  (svr_to_dac * dac_to_fft * fft_to_out) + (svr_to_fft * fft_to_dac * dac_to_out)

let () = Printf.printf "%d\n" second_part
