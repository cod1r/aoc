import Debug.Trace
import Data.List
import System.IO

canMove instruction (startX, startY) empty boxes walls =
  case instruction of
    '^' ->
      let steps = takeWhile (\thing -> not ((startX, startY - thing) `elem` walls)) [1..] in
      any (\n -> (startX, startY - n) `elem` empty) steps
    '>' ->
      let steps = takeWhile (\thing -> not ((startX + thing, startY) `elem` walls)) [1..] in
      any (\n -> (startX + n, startY) `elem` empty) steps
    'v' ->
      let steps = takeWhile (\thing -> not ((startX, startY + thing) `elem` walls)) [1..] in
      any (\n -> (startX, startY + n) `elem` empty) steps
    '<' ->
      let steps = takeWhile (\thing -> not ((startX - thing, startY) `elem` walls)) [1..] in
      any (\n -> (startX - n, startY) `elem` empty) steps

getNewPoint (x, y) instruction amt =
  case instruction of
    '^' ->
      (x, y - amt)
    '>' ->
      (x + amt, y)
    'v' ->
      (x, y + amt)
    '<' ->
      (x - amt, y)

move instructions empty boxes walls (rx, ry) =
  if length instructions == 0 then
    boxes
  else
    let (h:t) = instructions in
    if canMove h (rx, ry) empty boxes walls then
      let amts = takeWhile (\n ->
                  let newPoint = getNewPoint (rx, ry) h n in
                  not (newPoint `elem` empty)
                  ) [1..] in
      let relevantBoxes = map (\n -> getNewPoint (rx, ry) h n) amts in
      let newBoxes = map (\p -> if p `elem` relevantBoxes then getNewPoint p h 1 else p) boxes in
      if length relevantBoxes > 0 then
        let removedEmpty = getNewPoint (rx, ry) h (length relevantBoxes + 1) in
        let newEmpty = map (\p -> if p == removedEmpty then (rx, ry) else p) empty in
        move t newEmpty newBoxes walls (getNewPoint (rx, ry) h 1)
      else
        let robotPos = getNewPoint (rx, ry) h 1 in
        let newEmpty = map (\p -> if p == robotPos then (rx, ry) else p) empty in
        move t newEmpty newBoxes walls robotPos
    else
      move t empty boxes walls (rx, ry)

main = do
  handle <- openFile "day15.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let sections = groupBy (\a b -> a /= "" && b /= "") splitLines
  let firstSection = concat $ (take 1 sections)
  let secondSection = (drop 2 sections)
  let innerZipped = map (\l -> zip [1..length l] l) firstSection
  let zipped = zip [1..length firstSection] innerZipped
  let walls = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == '#') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped
  let boxes = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == 'O') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped
  let empty = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == '.') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped
  let (robot:_) = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == '@') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped
  let instructions = concat (concat secondSection)
  let finalBoxes = move instructions empty boxes walls robot
  let ans = foldr (\(x, y) sum -> sum + 100 * (y - 1) + (x - 1)) 0 finalBoxes
  print ans
  hClose handle
