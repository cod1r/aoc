import Data.Ord
import Data.Maybe
import Data.Array
import Data.List
import Debug.Trace
import System.IO

type TwoDArray = Array Int (Array Int Char)

buildArray :: [String] -> TwoDArray
buildArray lst =
  let mapped = map (\line -> array (1, length line) (zip [1..length line] line)) lst in
    array (1, length mapped) (zip [1..length mapped] mapped)

getValueAtPoint :: (Int, Int) -> TwoDArray -> Maybe Char
getValueAtPoint (x, y) array =
  let (start, end) = bounds array in
    if y >= start && y <= end then
      let row = array ! y in
      let (colStart, colEnd) = bounds row in
        if x >= colStart && x <= colEnd then
          Just (row ! x)
        else
          Nothing
    else
      Nothing

getAntiPoints (px, py) others =
  map (\(p2x, p2y) -> (px + (px - p2x), py + (py - p2y))) others

getAntiPointsPart2 (px, py) others =
  map (\(p2x, p2y) -> map (\m -> (px + m * (px - p2x), py + m * (py - p2y))) [0..]) others

getOrdering a b array =
  let ac = fromJust (getValueAtPoint a array) in
    let bc = fromJust (getValueAtPoint b array) in
    if ac < bc then
      Data.Ord.LT
    else if bc < ac then
      Data.Ord.GT
    else
      Data.Ord.EQ

main = do
  handle <- openFile "day8.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let array = buildArray splitLines
  let antennas = concat (map (\(rIdx, r) -> map (\(cIdx, _) -> (cIdx, rIdx)) (filter (\(_, c) -> c /= '.') (assocs r))) (assocs array))
  let sorted = sortBy (\p p2 -> getOrdering p p2 array) antennas
  let grouped = groupBy (\a b -> getValueAtPoint a array == getValueAtPoint b array) sorted

  let maybeAntiNodes = concat $ concat (map (\g -> map (\p -> getAntiPoints p (filter (\p2 -> p2 /= p) g)) g) grouped)
  let inBounds = filter (\p -> isJust $ getValueAtPoint p array) maybeAntiNodes
  let unique = foldr (\x acc -> if not (x `elem` acc) then x:acc else acc) [] inBounds
  putStrLn (show $ length unique)

  let maybeAntiNodesPart2 = (map (\g -> map (\p -> concat $ map (takeWhile (\newPoint -> isJust $ getValueAtPoint newPoint array)) (getAntiPointsPart2 p (filter (\p2 -> p2 /= p) g))) g) grouped)
  let flattened = (concat . concat $ maybeAntiNodesPart2)
  let unique2 = foldr (\x acc -> if not (x `elem` acc) then x:acc else acc) [] flattened
  print (length unique2)
  hClose handle
