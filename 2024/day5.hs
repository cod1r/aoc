import Data.List
import Data.Maybe
import Data.Ord
import System.IO
import Debug.Trace

readNum :: String -> Int
readNum n = read n

buildTupleList :: [String] -> [(Int, Int)]
buildTupleList lst =
  let mapped = Data.List.map (\x -> let firstNum = takeWhile (\c -> c /= '|') x in (readNum firstNum, readNum (drop (length firstNum + 1) x))) lst in
  Data.List.foldr (:) [] mapped

-- extractNumbers :: [(Int, Int)] -> [Int]
-- extractNumbers lst =
--   Data.List.foldr (\(f, s) acc -> acc ++ [f, s]) [] lst

sortFn :: [(Int, Int)] -> Int -> Int -> Ordering
sortFn ordering n1 n2 =
  case (Data.List.find (\(f, s) -> (f == n1 && s == n2) || (f == n2 && s == n1))) ordering of
    (Just (f, s)) -> if f == n1 && s == n2 then Data.Ord.LT else Data.Ord.GT
    Nothing -> if n1 == n2 then Data.Ord.EQ else error ("Direct mapping not found." ++ (show $ (n1, n2)))

-- buildSortedList :: [(Int, Int)] -> [Int]
-- buildSortedList lst =
--   let allNums = extractNumbers lst in
--   Data.List.sortBy (sortFn lst) allNums

parseCSV :: String -> [Int]
parseCSV s =
  if length s > 0 then
  let num = takeWhile (\c -> c /= ',') s in
  [readNum num] ++ parseCSV (drop (length num + 1) s)
  else []

buildNumLst :: [String] -> [[Int]]
buildNumLst lst =
  Data.List.map (\s -> parseCSV s) lst

main = do
  handle <- openFile "day5.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let firstSection = takeWhile (\x -> x /= "") splitLines
  let secondSection = drop (length firstSection + 1) splitLines
  let tupleLst = buildTupleList firstSection
  let updates = buildNumLst secondSection
  let valid = Data.List.filter (\l -> (Data.List.sortBy (sortFn tupleLst) l == l)) updates
  let invalid = Data.List.filter (\l -> (Data.List.sortBy (sortFn tupleLst) l /= l)) updates
  let corrected = Data.List.map (\l -> Data.List.sortBy (sortFn tupleLst) l) invalid
  let ans = Data.List.foldr (\l acc -> acc + (l !! (length l `div` 2))) 0 valid
  let ans2 = Data.List.foldr (\l acc -> acc + (l !! (length l `div` 2))) 0 corrected
  putStrLn (show ans)
  putStrLn (show ans2)
  hClose handle
