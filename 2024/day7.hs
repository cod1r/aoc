import System.IO
import Data.List
import Data.Char
import Debug.Trace

minInt :: Int
minInt = minBound

maxInt :: Int
maxInt = maxBound

readNum :: String -> Int
readNum n = read n

isNum n =
  let ascii = ord n in ascii >= 48 && ascii <= 57

parseListNum line = 
  let (targetStr, numList) = foldr (\c (s, l) -> if isNum c then (c:s, l) else if length s > 0 then ("", readNum s : l) else (s, l)) ("", []) line in
    readNum targetStr : numList

lengthNum n count =
  if n > 0 then
    lengthNum (n `div` 10) (count + 1)
  else
    count

valid :: Int -> [Int] -> Bool
valid target numbers =
  if length numbers == 1 then
    let (result:_) = numbers in
    result == target
  else
    let (first:second:tail) = numbers in
    valid target (first + second : tail) || valid target (first * second : tail)

valid2 :: Int -> [Int] -> Bool
valid2 target numbers =
  if length numbers == 1 then
    let (result:_) = numbers in
    result == target
  else
    let (first:second:tail) = numbers in
    valid2 target (first + second : tail) || valid2 target (first * second : tail) ||
      valid2 target (first *  10 ^ (lengthNum second 0) + second : tail)

main = do
  handle <- openFile "day7.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let parsed = map (\l -> parseListNum l) splitLines
  let validOrNot = map (\(h:t) -> valid h t) parsed
  let ans = foldr (\(target:_, valid) acc -> if valid then acc + target else acc) 0 (zip parsed validOrNot)
  putStrLn (show ans)
  let validOrNot2 = map (\(h:t) -> valid2 h t) parsed
  let ans2 = foldr (\(target:_, valid) acc -> if valid then acc + target else acc) 0 (zip parsed validOrNot2)
  putStrLn (show ans2)
  hClose handle
