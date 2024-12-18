import Data.Maybe
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

data WideBox =
  WideBox {
    leftSide :: Int,
    rightSide :: Int,
    row :: Int
  }
  deriving (Show, Eq)

getNewWideBox instruction WideBox { leftSide, rightSide, row } =
  case instruction of
    '^' ->
      WideBox { leftSide = leftSide, rightSide = rightSide, row = row - 1 }
    'v' ->
      WideBox { leftSide = leftSide, rightSide = rightSide, row = row + 1 }
    '<' ->
      WideBox { leftSide = leftSide - 1, rightSide = rightSide - 1, row = row }
    '>' ->
      WideBox { leftSide = leftSide + 1, rightSide = rightSide + 1, row = row }

getNextBox instruction boxes (x, y) =
  case instruction of
    '^' ->
      find (\WideBox { leftSide, rightSide, row } -> x>=leftSide&&x<=rightSide&&row==y - 1) boxes
    'v' ->
      find (\WideBox { leftSide, rightSide, row } -> x>=leftSide&&x<=rightSide&&row==y + 1) boxes
    '<' ->
      find (\WideBox { leftSide, rightSide, row } -> x-1==rightSide&&row==y) boxes
    '>' ->
      find (\WideBox { leftSide, rightSide, row } -> x+1==leftSide&&row==y) boxes

getBoxesInWay currentBox boxes instruction =
  let WideBox{leftSide=cLeftSide,rightSide=cRightSide,row=cRow} = currentBox in
  case instruction of
    '^' ->
      let filtered = filter (\WideBox{leftSide,rightSide,row} -> leftSide<=cRightSide&&cLeftSide<=rightSide&&row==cRow - 1) boxes in
      filtered ++ foldr (\b acc -> (getBoxesInWay b boxes instruction)++acc) [] filtered
    'v' ->
      let filtered = filter (\WideBox{leftSide,rightSide,row} -> leftSide<=cRightSide&&cLeftSide<=rightSide&&row==cRow + 1) boxes in
      filtered ++ foldr (\b acc -> (getBoxesInWay b boxes instruction)++acc) [] filtered
    '<' ->
      let filtered = filter (\WideBox{leftSide,rightSide,row} -> rightSide == cLeftSide - 1 && row == cRow) boxes in
      filtered ++ foldr (\b acc -> (getBoxesInWay b boxes instruction)++acc) [] filtered
    '>' ->
      let filtered = filter (\WideBox{leftSide,rightSide,row} -> leftSide == cRightSide + 1 && row == cRow) boxes in
      filtered ++ foldr (\b acc -> (getBoxesInWay b boxes instruction)++acc) [] filtered

getWallsInWay box walls instruction =
  let WideBox{leftSide=cLeftSide,rightSide=cRightSide,row=cRow} = box in
  case instruction of
    '^' ->
      filter (\(cidx, ridx) -> cidx>=cLeftSide&&cidx<=cRightSide&&ridx==cRow - 1)walls
    'v' ->
      filter (\(cidx, ridx) -> cidx>=cLeftSide&&cidx<=cRightSide&&ridx==cRow + 1)walls
    '<' ->
      filter (\(cidx, ridx) -> cidx == cLeftSide - 1 && ridx == cRow)walls
    '>' ->
      filter (\(cidx, ridx) -> cidx == cRightSide + 1 && ridx == cRow)walls

moveWithWideBox instructions boxes walls (rx, ry) =
  if length instructions == 0 then
    boxes
  else
    let (h:t) = instructions in
    let nextBox = getNextBox h boxes (rx, ry) in
    let nextPoint = getNewPoint (rx, ry) h 1 in
    if isJust nextBox then
      let unwrappedNextBox = fromJust nextBox in
      let boxesInWay = unwrappedNextBox:(getBoxesInWay (unwrappedNextBox) boxes h) in
      let wallsInWay = foldr (\b acc -> (getWallsInWay b walls h)++acc) [] boxesInWay in
      if length wallsInWay == 0 then
        let updatedBoxes = map (\b -> if b `elem` boxesInWay then (getNewWideBox h b) else b) boxes in
        moveWithWideBox t updatedBoxes walls (getNewPoint (rx, ry) h 1)
      else
        moveWithWideBox t boxes walls (rx, ry)
    else
      moveWithWideBox t boxes walls (if nextPoint `elem` walls then (rx, ry) else nextPoint)

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

  let firstSectionExpanded = map (\l ->
                              foldr (\c acc ->
                                if c == '@' then
                                  [c, '.'] ++ acc
                                else if c == 'O' then
                                  ['[', ']'] ++ acc
                                else
                                  [c, c] ++ acc
                              ) [] l
                            ) firstSection
  -- let visualized = foldr (\l acc -> l ++ "\n" ++ acc) "" firstSectionExpanded
  -- putStrLn visualized

  let innerZipped = map (\l -> zip [1..length l] l) firstSectionExpanded
  let zipped = zip [1..length firstSectionExpanded] innerZipped
  let walls = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == '#') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped
  let boxes = foldr (\(ridx, r) acc ->
                let grouped = groupBy (\(_, a) (_, b) -> a == '[' && b == ']') r in
                let filtered = filter (\l -> length l == 2) grouped in
                let wideboxes = map (\((cidx, _):(cidx2, _):t) ->
                                  WideBox { leftSide = cidx, rightSide = cidx2, row = ridx }) filtered in
                acc ++ wideboxes
                ) [] zipped
  let empty = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == '.') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped
  let (robot:_) = foldr (\(ridx, r) acc ->
                let filtered = filter (\(_, c) -> c == '@') r in
                acc ++ map (\(cidx, _) -> (cidx, ridx)) filtered
                ) [] zipped

  let finalBoxes = moveWithWideBox instructions boxes walls robot
  let ans2 = foldr (\WideBox { leftSide, row } sum ->
              sum + 100 * (row - 1) + (leftSide - 1)) 0 finalBoxes
  print ans2
  hClose handle
