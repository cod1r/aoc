import Data.Ord
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Array
import System.IO
import Data.List
import Debug.Trace

buildArr :: [String] -> Array Int (Array Int Char)
buildArr lines =
  let lstArrays = map (\l -> array (1, length l) (zip [1..length l] l)) lines in
  array (1, length lstArrays) (zip [1..length lstArrays] lstArrays)

directions = [ (0, 1), (0, -1), (1, 0), (-1, 0) ]

getInnerValue :: Array Int (Array Int Char) -> (Int, Int) -> Maybe Char
getInnerValue arr (x, y) =
  let (start, end) = bounds arr in
  if y >= start && y <= end then
    let row = arr ! y in
    let (startCol, endCol) = bounds row in
    if x >= startCol && x <= endCol then
      Just (row ! x)
    else
      Nothing
  else
    Nothing

groupRegion :: Char -> (Int, Int) -> Array Int (Array Int Char) -> Map.Map (Int, Int) Bool -> Map.Map (Int, Int) Bool
groupRegion region (x, y) arr visited =
  let appliedDirections = map (\(dx, dy) -> (dx + x, dy + y)) directions in
  let coords = filter (\(newX, newY) ->
                  let result = getInnerValue arr (newX, newY) in
                  isJust result && fromJust result == region && not (isJust $ Map.lookup (newX, newY) visited)
                ) appliedDirections in
  let updatedVisited = (Map.insert (x, y) True visited) in
  foldr (\p acc -> groupRegion region p arr acc) updatedVisited coords

traverseRegion :: Array Int (Array Int Char) -> Map.Map (Int, Int) Bool -> Int
traverseRegion arr points =
  Map.foldrWithKey (\(x, y) _ sum ->
    let appliedDirections = map (\(dx, dy) -> (dx + x, dy + y)) directions in
    let region = fromJust (getInnerValue arr (x, y)) in
    sum + foldr (\p acc ->
            let c = getInnerValue arr p in
            acc + if not (isJust c) || fromJust c /= region then 1 else 0
          ) 0 appliedDirections
    ) 0 points

main = do
  handle <- openFile "day12.input" ReadMode
  contents <- hGetContents handle
  let arr = buildArr (lines contents)
  let points = concat $ map (\(ridx, r) -> foldr (\(cidx, c) acc -> (cidx, ridx):acc) [] (assocs r)) (assocs arr)
  let regions = foldr (\p acc ->
                  let c = fromJust (getInnerValue arr p) in
                  if any (\(_, pm) -> let looked = Map.lookup p pm in isJust looked) acc then
                    acc
                  else
                    (c, groupRegion c p arr Map.empty):acc
                ) [] points
  let sorted = sortBy (\(k, _) (k2, _) -> if k < k2 then Data.Ord.LT else if k > k2 then Data.Ord.GT else Data.Ord.EQ) regions
  let grouped = groupBy (\(a, _) (b, _) -> a == b) sorted
  let uniqueInGroups = map (\g ->
                          foldr (\e acc -> if e `elem` acc then acc else e:acc) [] (map (\e -> snd e) g)
                          ) grouped
  let ans = foldr (\g acc -> acc + foldr (\m regionPrice -> regionPrice + traverseRegion arr m * Map.size m) 0 g) 0 uniqueInGroups
  print ans
  hClose handle
