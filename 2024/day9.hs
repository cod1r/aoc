import Data.Char
import Data.List
import System.IO

readNum :: Char -> Int
readNum n = ord n - ord '0'

main = do
  handle <- openFile "day9.input" ReadMode
  contents <- hGetContents handle
  let zipped = zip [1..length contents] (map (\c -> readNum c) contents)
  let represents = concat $ foldr (\(idx, space) acc -> (if idx `mod` 2 == 0 then take space $ repeat (-1) else take space $ repeat ((idx - 1) `div` 2)):acc) [] zipped
  let zippedRepresents = zip [1..length represents] represents
  let reversed = reverse $ filter (\(_, c) -> c /= -1) zippedRepresents
  let (_, built) = foldr (\(idx, space) ((idx2, h):t, acc) -> if idx2 < idx then ((idx2, h):t, -1:acc) else if space /= -1 then ((idx2, h):t, space:acc) else (t, h:acc)) (reversed, []) (reverse zippedRepresents)
  let anotherZip = filter (\(_, c) -> c /= -1) (zip [0..length built - 1] (reverse built))
  let ans = foldr (\(idx, id) acc -> acc + idx * id) 0 anotherZip
  print ans
  hClose handle
