import System.IO
import Data.Maybe

readNum :: String -> Int
readNum n = read n

customFoldFnInc n acc =
  let (valid, prevElement) = acc in
  if isJust prevElement then
    let pn = fromJust prevElement in
    let diff = pn - n in
    (valid && diff >= 1 && diff <= 3, Just n)
  else
    (valid, Just n)

customFoldFnDec n acc =
  let (valid, prevElement) = acc in
  if isJust prevElement then
    let pn = fromJust prevElement in
    let diff = pn - n in
    (valid && diff >= -3 && diff <= -1, Just n)
  else
    (valid, Just n)

everythingBut :: [Int] -> [[Int]]
everythingBut lst =
  let withIndices = zip lst [1..length lst] in
  let filtered = map (\a -> filter (\b -> snd a /= snd b) withIndices) withIndices in
  map (\lst -> map fst lst) filtered

isValid :: [Int] -> Bool
isValid lst =
 fst (foldr customFoldFnInc (True, Nothing) lst) || fst (foldr customFoldFnDec (True, Nothing) lst)

main = do
  handle <- openFile "day2.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let splitWhitespace = map words splitLines
  let numbersParsed = map (\x -> map readNum x) splitWhitespace
  let booled = map isValid numbersParsed
  let ans = length $ filter (\x -> x == True) booled
  putStrLn (show ans)
  let exploded = map everythingBut numbersParsed
  let mapped = map (\lst -> any isValid lst) exploded
  let truthy = filter (\x -> x == True) mapped
  putStrLn (show $ length truthy)
  hClose handle
