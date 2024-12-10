import Data.Char
import Data.Maybe
import Data.Array
import System.IO
import Debug.Trace

buildArray lines =
  let mapped = map (\l -> array (1, length l) (zip [1..length l] l)) lines in
  array (1, length mapped) (zip [1..length mapped] mapped)

getValueAtPoint (x, y) array =
  let (start, end) = bounds array in
    if y >= start && y <= end then
      let row = array ! y in
      let (startCol, endCol) = bounds row in
      if x >= startCol && x <= endCol then
        Just (row ! x)
      else
        Nothing
    else
      Nothing

traverseArr (x, y) array =
  let value = getValueAtPoint (x, y) array in
  if isJust value then
    let inner = fromJust value in
    if inner == '9' then
      [(x, y)]
    else
      let oneHigher = (intToDigit (ord inner - ord '0' + 1)) in
      let nexts = filter (\p -> let v = getValueAtPoint p array in isJust v && fromJust v == oneHigher)
                    [(x + 1, y),
                    (x - 1, y),
                    (x, y + 1),
                    (x, y - 1)] in
      foldr (\p acc -> acc ++ traverseArr p array) [] nexts
  else
    []

main = do
  handle <- openFile "day10.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let array = buildArray splitLines
  let zeroes = foldr (\(rowIdx, row) acc -> acc ++ map (\(colIdx, _) -> (colIdx, rowIdx)) (filter (\(_, c) -> c == '0') (assocs row))) [] (assocs array)
  let traversed = map (\z -> length $ foldr (\x acc -> if x `elem` acc then acc else x:acc) [] (traverseArr z array)) zeroes
  let ans = foldr (\x acc -> acc + x) 0 traversed
  print ans
  let traversed2 = map (\z -> length (traverseArr z array)) zeroes
  let ans2 = foldr (\x acc -> acc + x) 0 traversed2
  print ans2
  hClose handle
