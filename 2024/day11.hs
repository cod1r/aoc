import Data.Maybe
import Debug.Trace
import Data.List
import System.IO

readNum :: String -> Int
readNum n = read n

lengthNum n =
  if n > 0 then 1 + lengthNum (n `div` 10) else 0

buildList :: String -> [Int]
buildList s =
  map (\g -> readNum g) (filter (\g -> not (' ' `elem` g)) (groupBy (\a b -> a /= ' ' && b /= ' ') s))

applyRules n =
  case n of
    _ | n == 0 -> [1]
    _ | lengthNum n `mod` 2 == 0 -> let halfLen = lengthNum n `div` 2 in [n `div` (10 ^ halfLen), n `mod` (10 ^ halfLen)]
    _ -> [n * 2024]

blink timesBlinked lst limit =
  if timesBlinked == limit then
    lst
  else
    let afterBlink = concat $ map applyRules lst in
    blink (timesBlinked + 1) afterBlink limit

main = do
  handle <- openFile "day11.input" ReadMode
  contents <- hGetContents handle
  let lst = buildList contents
  let ans = blink 0 lst 25
  print (length ans)
  hClose handle
