-- compiled with ghci -package containers file.hs

import Data.IntMap
import Data.List
import Data.Maybe
import Data.Set
import System.IO
import Debug.Trace

buildIntMap :: [(Int, [(Int, Char)])] -> IntMap (IntMap Char)
buildIntMap lst =
  let innerIntMaps = Data.List.map (\x -> (fst x, Data.IntMap.fromList (snd x))) lst
   in Data.IntMap.fromList innerIntMaps

horizontal :: IntMap Char -> String -> Int -> Int -> Int
horizontal im target base acc =
  let lookups = [Data.IntMap.lookup base im, Data.IntMap.lookup (base + 1) im, Data.IntMap.lookup (base + 2) im, Data.IntMap.lookup (base + 3) im]
   in if any isNothing lookups
        then
          acc
        else
          let unwrapped = Data.List.map fromJust lookups
           in if unwrapped == target
                then
                  horizontal im target (base + 1) (acc + 1)
                else
                  horizontal im target (base + 1) (acc)

vertical :: IntMap (IntMap Char) -> String -> Int -> Int -> Int -> Int
vertical im target base col acc =
  let lookups = [Data.IntMap.lookup base im, Data.IntMap.lookup (base + 1) im, Data.IntMap.lookup (base + 2) im, Data.IntMap.lookup (base + 3) im]
   in if any isNothing lookups
        then
          acc
        else
          let [firstRow, secondRow, thirdRow, fourthRow] = Data.List.map fromJust lookups
           in let columnLookups = [Data.IntMap.lookup col firstRow, Data.IntMap.lookup col secondRow, Data.IntMap.lookup col thirdRow, Data.IntMap.lookup col fourthRow]
               in let unwrapped = Data.List.map fromJust columnLookups
                   in if unwrapped == target
                        then
                          vertical im target (base + 1) col (acc + 1)
                        else
                          vertical im target (base + 1) col (acc)

genDiagonalPoints :: (Int, Int) -> [[(Int, Int)]]
genDiagonalPoints (first, second) =
  [ (first, second) : (Data.List.map (\x -> (x + first, x + second)) [1 .. 3]),
    (first, second) : (Data.List.map (\x -> (x + first, second - x)) [1 .. 3]),
    (first, second) : (Data.List.map (\x -> (first - x, second - x)) [1 .. 3]),
    (first, second) : (Data.List.map (\x -> (first - x, x + second)) [1 .. 3])
  ]

genDiagonalPointsPart2 :: (Int, Int) -> [[(Int, Int)]]
genDiagonalPointsPart2 (first, second) =
  [ [(first, second) , (1 + first, 1 + second)],
    [(first, second) , (1 + first, second - 1)],
    [(first, second) , (first - 1, second - 1)],
    [(first, second) , (first - 1, 1 + second)]
  ]

getNestedValue :: IntMap (IntMap Char) -> (Int, Int) -> Char
getNestedValue im (first, second) =
  let lookedRes = Data.IntMap.lookup second im
   in if isJust lookedRes
        then
          let innerIM = fromJust lookedRes
           in let value = Data.IntMap.lookup first innerIM
               in if isJust value
                    then
                      fromJust value
                    else
                      ' '
        else
          ' '

checkPoints :: IntMap (IntMap Char) -> [(Int, Int)] -> String -> Bool
checkPoints im points target =
  let values = (Data.List.map (\p -> getNestedValue im p) points) in
    target == values

checkPointsPart2 :: IntMap (IntMap Char) -> [(Int, Int)] -> String -> Bool
checkPointsPart2 im points target =
  let values = (Data.List.map (\p -> getNestedValue im p) points) in
    target == values

oneOf :: [String] -> Bool
oneOf group =
  group == ["AS","AS","AM","AM"] ||
  group == ["AM","AM","AS","AS"] ||
  group == ["AS","AM","AM","AS"] ||
  group == ["AM","AS","AS","AM"]

main = do
  handle <- openFile "day4.input" ReadMode
  contents <- hGetContents handle
  let splitLines = lines contents
  let innerZipped = Data.List.map (\x -> zip [1 .. (length x)] x) splitLines
  let zipped = zip [1 .. length (innerZipped)] innerZipped
  let intmap = buildIntMap zipped
  let leftToRight = Data.IntMap.map (\x -> horizontal x "XMAS" 1 0) intmap
  let rightToLeft = Data.IntMap.map (\x -> horizontal x "SAMX" 1 0) intmap
  let lenFirstRow = length $ fromJust $ Data.IntMap.lookup 1 intmap
  let totalLTR = Data.IntMap.foldr (\x acc -> acc + x) 0 leftToRight
  let totalRTL = Data.IntMap.foldr (\x acc -> acc + x) 0 rightToLeft
  let topDown = Data.List.map (\x -> vertical intmap "XMAS" 1 x 0) [1 .. lenFirstRow]
  let totalTopDown = Data.List.foldr (\x acc -> acc + x) 0 topDown
  let downTop = Data.List.map (\x -> vertical intmap "SAMX" 1 x 0) [1 .. lenFirstRow]
  let totalDownTop = Data.List.foldr (\x acc -> acc + x) 0 downTop
  let points = Data.List.foldr (\x acc -> acc ++ x) [] (Data.List.map (\x -> Data.List.map (\y -> (x, y)) [1 .. lenFirstRow]) [1 .. lenFirstRow])
  let diagonalGroups = Data.List.foldr (\x acc -> acc ++ genDiagonalPoints x) [] points
  let diags = length $ Data.List.filter (\x -> x == True) (Data.List.map (\x -> checkPoints intmap x "XMAS") diagonalGroups)
  putStrLn (show $ diags + totalRTL + totalLTR + totalTopDown + totalDownTop)
  let diagonalGroups2 = Data.List.map (\x -> genDiagonalPointsPart2 x) points
  let mapped = Data.List.map (\x -> Data.List.map (\y -> Data.List.map (\z -> getNestedValue intmap z) y) x) diagonalGroups2
  let ans2 = length $ Data.List.filter (\x -> oneOf x) mapped
  putStrLn (show ans2)
  hClose handle
