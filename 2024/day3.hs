import System.IO
import Data.Char
import Data.Maybe
import Debug.Trace

readNum :: String -> Int
readNum n = read n

isNum c =
  let ascii = ord c in
  ascii >= 48 && ascii <= 57

isComma (c:_) =
  let ascii = ord c in
  ascii == 44

isClosingParenth (c:_) =
 let ascii = ord c in
 ascii == 41

handleNum :: String -> (String, Int)
handleNum s =
  let betweenFirstAndNonNum = takeWhile isNum s in
  if length betweenFirstAndNonNum == 0 then
  let num = lookForMul betweenFirstAndNonNum 0 Nothing in
    (s, num)
  else
    (drop (length betweenFirstAndNonNum) s, readNum betweenFirstAndNonNum)

parseMul :: String -> (String, Int)
parseMul s =
  let (remaining, leftNum) = handleNum s in
  if isComma (take 1 remaining) then
    let (remainingAfterRight, rightNum) = handleNum (drop 1 remaining) in
    if isClosingParenth (take 1 remainingAfterRight) then
      (drop 1 remainingAfterRight, leftNum * rightNum)
    else
      (s, 0)
  else
    (s, 0)

lookForMul :: String -> Int -> Maybe Bool -> Int
lookForMul s acc enabled =
  if (length s) >= 4 then
    let (first:second:third:fourth:t) = s in
    if [first,second,third,fourth] == "mul(" then
      let (remaining, res) = parseMul t in
      if isJust enabled then
        let innerEnabled = fromJust enabled in
        lookForMul remaining (acc + if innerEnabled then res else 0) enabled
      else
        lookForMul remaining (acc + res) enabled
    else if [first,second,third,fourth] == "do()" && isJust enabled then
      lookForMul t acc (Just True)
    else
      let (fifth:six:seven:tt) = t in
      if [first,second,third,fourth,fifth,six,seven] == "don't()" && isJust enabled then
        lookForMul t acc (Just False)
      else
        lookForMul ([second,third,fourth] ++ t) acc enabled
  else
    acc

main = do
  handle <- openFile "day3.input" ReadMode
  contents <- hGetContents handle
  let ans = lookForMul contents 0 Nothing
  putStrLn (show ans)
  let ans2 = lookForMul contents 0 (Just True)
  putStrLn (show ans2)
  hClose handle
