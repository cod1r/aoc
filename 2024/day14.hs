import Data.Maybe
import Data.List
import Data.Ord
import Data.Array
import Debug.Trace
import Data.Char
import System.IO

readNum :: String -> Int
readNum n = read n

isNum c =
  let ascii = ord c in
  ascii >= 48 && ascii <= 57

data Robot =
  Robot {
    x :: Int,
    y :: Int,
    dx :: Int,
    dy :: Int
  }
  deriving (Show)

parseTwoNumbers :: String -> (Int, Int)
parseTwoNumbers line =
  let numStart = dropWhile (\c -> not (isNum c) && c /= '-') line in
  let x = takeWhile (\c -> isNum c || c == '-') numStart in
  let rest = (drop (length x + 1) numStart) in
  let y = takeWhile (\c -> isNum c || c == '-') rest in
  (readNum x, readNum y)

buildString robots num_rows num_cols =
  let lstStrings = [let filtered = filter (\Robot { y } -> y == r) robots in
                                    [ let found = find (\Robot { x } -> x == c) filtered in if isJust found then '*' else '.' | c <- [0..num_cols - 1]] | r <- [0..num_rows - 1]
                    ] in
  foldr (\s acc -> s ++ "\n" ++ acc) "" lstStrings

main = do
  handle <- openFile "day14.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let num_rows = 103
  let num_cols = 101
  let robots = map (\l ->
                  let firstPart = takeWhile (\c -> c /= ' ') l in
                  let secondPart = drop (length firstPart) l in
                  let (x, y) = parseTwoNumbers firstPart in
                  let (dx, dy) = parseTwoNumbers secondPart in
                  Robot { x = x, y = y, dx = dx, dy = dy }
                ) splitLines
  let afterHundred = map (\r -> let Robot { x, y, dx, dy } = r in Robot { x = (x + dx * 100) `mod` num_cols, y = (y + dy * 100) `mod` num_rows, dx, dy }) robots
  let afterHundredFiltered = filter (\r -> let Robot { x, y } = r in x /= num_cols `div` 2 && y /= num_rows `div` 2) afterHundred
  let firstQuad = filter (\r -> let Robot { x, y } = r in x < num_cols `div` 2 && y < num_rows `div` 2) afterHundredFiltered
  let secondQuad = filter (\r -> let Robot { x, y } = r in x > num_cols `div` 2 && y < num_rows `div` 2) afterHundredFiltered
  let thirdQuad = filter (\r -> let Robot { x, y } = r in x < num_cols `div` 2 && y > num_rows `div` 2) afterHundredFiltered
  let fourthQuad = filter (\r -> let Robot { x, y } = r in x > num_cols `div` 2 && y > num_rows `div` 2) afterHundredFiltered
  let ans = length firstQuad * length secondQuad * length thirdQuad * length fourthQuad
  print ans
  let range = [0..50000]
  let nRobots = map (\n ->
                  let newRobots = map (\Robot { x, y, dx, dy } ->
                                    Robot { x = (x + dx * n) `mod` num_cols, y = (y + dy * n) `mod` num_rows, dx = dx, dy = dy }) robots in
                  buildString newRobots num_rows num_cols
                ) range
  mapM (\(i, s) -> putStrLn (show i ++ "\n" ++ s)) (zip range nRobots)
  hClose handle
