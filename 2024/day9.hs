import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

readNum :: Char -> Int
readNum n = ord n - ord '0'

getProperGroup h group =
  let (_, acc) = foldr (\n (rest, acc) -> if length rest == 0 then (rest, n : acc) else if n /= -1 then (rest, n : acc) else let (head : t) = rest in (t, head : acc)) (h, []) group
   in acc

canPlace (idxOfGroup, group) before =
  foldr
    ( \(idx, g) acc ->
        if length (filter (\n -> n == -1) g) >= length group && idx < idxOfGroup
          then
            Just idx
          else if isJust acc then acc else Nothing
    )
    Nothing
    before

tryMove fileBlocks ans =
  if length fileBlocks > 0
    then
      let ((idxh, h) : t) = fileBlocks
       in if all (\n -> n == -1) h
            then
              tryMove t ans
            else
              let use = if length ans == 0 then t else ans
               in let attempt = canPlace (idxh, h) (reverse use)
                   in if isJust attempt
                        then
                          let placeIdx = fromJust attempt
                           in let built = (foldr (\(idx, group) acc -> if idx == placeIdx then (idx, getProperGroup h group) : acc else (idx, if group == h then [-1 | _ <- [1 .. length group]] else group) : acc) [] use)
                               in tryMove t built
                        else
                          tryMove t ans
    else
      ans

main = do
  handle <- openFile "day9.input" ReadMode
  contents <- hGetContents handle
  let zipped = zip [1 .. length contents] (map (\c -> readNum c) contents)
  let represents = concat $ foldr (\(idx, space) acc -> (if idx `mod` 2 == 0 then take space $ repeat (-1) else take space $ repeat ((idx - 1) `div` 2)) : acc) [] zipped
  let zippedRepresents = zip [1 .. length represents] represents
  let reversed = reverse $ filter (\(_, c) -> c /= -1) zippedRepresents
  let (_, built) = foldr (\(idx, space) ((idx2, h) : t, acc) -> if idx2 < idx then ((idx2, h) : t, -1 : acc) else if space /= -1 then ((idx2, h) : t, space : acc) else (t, h : acc)) (reversed, []) (reverse zippedRepresents)
  let anotherZip = filter (\(_, c) -> c /= -1) (zip [0 .. length built - 1] (reverse built))
  let ans = foldr (\(idx, id) acc -> acc + idx * id) 0 anotherZip
  print ans
  let grouped = group represents
  let zippedGrouped = zip [1 .. length grouped] grouped
  let fileBlocks = reverse zippedGrouped
  let what = tryMove fileBlocks []
  let folded = concat $ map (snd) what
  let ans2 = foldr (\(pos, id) acc -> if id == -1 then acc else acc + id * pos) 0 (zip [0 .. length folded - 1] $ reverse folded)
  print ans2
  hClose handle
