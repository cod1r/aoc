import Data.Maybe
import Debug.Trace
import Data.Char
import Data.List
import System.IO

data Machine =
  Machine {
    a :: (Int, Int),
    b :: (Int, Int),
    p :: (Int, Int)
  }
  deriving (Show)

readNum :: String -> Int
readNum n = read n

isNum c =
  let ascii = ord c in
  ascii >= 48 && ascii <= 57

parseLine :: String -> (Int, Int)
parseLine s =
  let dropped = dropWhile (\c -> not (isNum c)) s in
  let firstNum = takeWhile (\c -> isNum c) dropped in
  let dropped2 = drop (length firstNum) dropped in
  let dropped3 = dropWhile (\c -> not (isNum c)) dropped2 in
  let secondNum = takeWhile (\c -> isNum c) dropped3 in
  (readNum firstNum, readNum secondNum)

parseChunk :: [String] -> Machine
parseChunk (a:b:p:_) =
  let buttonA = parseLine a in
  let buttonB = parseLine b in
  let prize = parseLine p in
  Machine { a=buttonA, b=buttonB, p=prize }

solveMachine :: Machine -> Maybe (Int, Int)
solveMachine Machine { a, b, p } =
  let (ax, ay) = a in
  let (bx, by) = b in
  let (px, py) = p in
  let ac = -(fromIntegral ay) / (fromIntegral ax) in
  let newBY = ac * (fromIntegral bx) + (fromIntegral by) in
  let newPY = ac * (fromIntegral px) + (fromIntegral py) in
  let buttonBPresses = newPY / newBY in
  let buttonAPresses = ((fromIntegral px) - (fromIntegral bx) * buttonBPresses) / (fromIntegral ax) in
  let appliedToX = (fromIntegral ax) * (round buttonAPresses) + (fromIntegral bx) * (round buttonBPresses) in
  let appliedToY = (fromIntegral ay) * (round buttonAPresses) + (fromIntegral by) * (round buttonBPresses) in
  if appliedToX == (fromIntegral px)
    && appliedToY == (fromIntegral py) then
    Just (round buttonAPresses, round buttonBPresses)
  else
    Nothing

main = do
  handle <- openFile "day13.input" ReadMode
  contents <- hGetContents handle
  let chunks = filter (\l -> length l > 1)
                (groupBy (\a b -> a /= "" && b /= "")
                  (lines contents))
  let machines = map (\chunk -> parseChunk chunk) chunks
  let applied = map (\m -> solveMachine m) machines
  let working = filter (\answer -> isJust answer) applied
  let ans = foldr (\answer acc -> let (ap, bp) = fromJust answer in acc + ap * 3 + bp) 0 working
  print ans
  let part2Machines = map (\m -> let Machine { a, b, p = (px, py) } = m in Machine { a = a, b = b, p = (px + 10000000000000, py + 10000000000000) }) machines
  let appliedPart2 = map (\m -> solveMachine m) part2Machines
  let workingPart2 = filter (\answer -> isJust answer) appliedPart2
  let ansPart2 = foldr (\answer acc -> let (ap, bp) = fromJust answer in acc + ap * 3 + bp) 0 workingPart2
  print ansPart2
  hClose handle
