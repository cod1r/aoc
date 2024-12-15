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
  hClose handle
