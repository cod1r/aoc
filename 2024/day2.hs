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
  hClose handle
