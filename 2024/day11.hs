import qualified Data.Map.Strict as Map
import Data.Ord
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

readNum :: String -> Int
readNum n = read n

lengthNum n =
  if n > 0 then 1 + lengthNum (n `div` 10) else 0

buildList :: String -> [Int]
buildList s =
  map (\g -> readNum g) (filter (\g -> not (' ' `elem` g)) (groupBy (\a b -> a /= ' ' && b /= ' ') s))

applyRules n =
  let len = lengthNum n
   in case n of
        _ | n == 0 -> [1]
        _ | len `mod` 2 == 0 -> let halfLen = len `div` 2 in [n `div` (10 ^ halfLen), n `mod` (10 ^ halfLen)]
        _ -> [n * 2024]

blink timesBlinked lst limit =
  if timesBlinked == limit
    then
      lst
    else
      let afterBlink = concat $ map applyRules lst in
      blink (timesBlinked + 1) afterBlink limit

blinkWithMaps dataMap timesBlinked limit =
  if timesBlinked == limit then
    dataMap
  else
    let newDataMap = Map.foldrWithKey (\k v acc ->
                        let newNums = applyRules k in
                        foldr (\n innerAcc ->
                          let found = Map.lookup n innerAcc in
                          if isJust found then
                            Map.adjust (+ v) n innerAcc
                          else
                            Map.insert n v innerAcc) acc newNums) Map.empty dataMap in
      blinkWithMaps newDataMap (timesBlinked + 1) limit

main = do
  handle <- openFile "day11.input" ReadMode
  contents <- hGetContents handle
  let lst = buildList contents
  let one = blink 0 lst 25
  print (length one)
  let mapCount = foldr (\k acc ->
                    let found = Map.lookup k acc in
                    if isJust found then
                      Map.adjust (+ 1) k acc
                    else
                      Map.insert k 1 acc) Map.empty lst
  let dataMap = blinkWithMaps mapCount 0 75
  let summed = Map.foldrWithKey (\k v acc -> acc + v) 0 dataMap
  print summed
  hClose handle
