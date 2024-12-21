import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map
import System.IO

data Direction = UP | DOWN | LEFT | RIGHT deriving (Enum, Show, Eq, Ord)

simpleTraverse :: Map.Map (Int, Int) () -> (Int, Int) -> Int -> Direction -> Map.Map (Int, Int) () -> (Int, Int) -> Map.Map (Int, Int, Direction) Int -> (Bool, Int, Map.Map (Int, Int, Direction) Int)
simpleTraverse walls (currentX, currentY) score dir visited destination outerCache =
  if (currentX, currentY) == destination then
    (True, score, outerCache)
  else
    let up = (currentX, currentY - 1, UP)
        down = (currentX, currentY + 1, DOWN)
        left = (currentX - 1, currentY, LEFT)
        right = (currentX + 1, currentY, RIGHT) in
    let filtered = filter (\(x, y, _) ->
                    not (isJust (Map.lookup (x, y) visited) || isJust (Map.lookup (x, y) walls))) [up, down, left, right] in
    let newVisited = Map.insert (currentX, currentY) () visited in
    let folded = foldr (\(x, y, d) (accF, accS, accC) ->
                  let lookupAttempt = Map.lookup (x, y, d) accC in
                  if isJust lookupAttempt then
                    (True, min accS (score + fromJust lookupAttempt), accC)
                  else
                    let (f, fs, innerCache) = simpleTraverse walls (x, y)
                                  (if dir == d then 1 else 1001)
                                  d
                                  newVisited
                                  destination
                                  accC in
                    if f then
                      (True, min accS (score + fs), Map.insert (x, y, d) fs innerCache)
                    else
                      (accF, accS, accC)
                  ) (False, maxBound::Int, outerCache) filtered in
    folded

main = do
  handle <- openFile "day16.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let zipped = zip [1..length splitLines] (map (\l -> zip [1..length l] l) splitLines)
  let walls = foldr (\(row, l) acc ->
                let filtered = filter (\(_, c) -> c == '#') l in
                acc ++ map (\(col, _) -> (col, row)) filtered
                ) [] zipped
  let tiles = foldr (\(row, l) acc ->
                let filtered = filter (\(_, c) -> c == '.') l in
                acc ++ map (\(col, _) -> (col, row)) filtered
                ) [] zipped
  let (s:_) = foldr (\(row, l) acc ->
                let filtered = filter (\(_, c) -> c == 'S') l in
                acc ++ map (\(col, _) -> (col, row)) filtered
                ) [] zipped
  let (e:_) = foldr (\(row, l) acc ->
                let filtered = filter (\(_, c) -> c == 'E') l in
                acc ++ map (\(col, _) -> (col, row)) filtered
                ) [] zipped
  let (_, fs, ans) = simpleTraverse (foldr (\p acc -> Map.insert p () acc) Map.empty walls) s 0 RIGHT Map.empty e Map.empty
  print fs
  hClose handle
