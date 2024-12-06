import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

buildNestedArray :: [String] -> Array Int (Array Int Char)
buildNestedArray lines =
  let lstOfArrays = map (\x -> array (1, length x) (zip [1 .. length x] x)) lines
   in array (1, length lstOfArrays) (zip [1 .. length lstOfArrays] lstOfArrays)

getInnerValue :: Array Int (Array Int Char) -> Int -> Int -> Maybe Char
getInnerValue nestedArray row col =
  let (start, end) = bounds nestedArray
   in if row >= start && row <= end
        then
          let arr = nestedArray ! row
           in let (startInner, endInner) = bounds arr
               in if col >= startInner && col <= endInner
                    then
                      Just (arr ! col)
                    else
                      Nothing
        else
          Nothing

move :: (Int, Int) -> [(Int, Int)] -> Array Int (Array Int Char) -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
move pos directions nestedArray visited obstacles =
  let ((addCol, addRow) : restOfDirections) = directions
   in let (col, row) = pos
       in let char = getInnerValue nestedArray (row + addRow) (col + addCol)
           in if isJust char
                then case fromJust char of
                  inner
                    | inner == '^' || inner == '.' ->
                        move
                          (col + addCol, row + addRow)
                          directions
                          nestedArray
                          (if (col + addCol, row + addRow) `elem` visited then visited else ((col + addCol, row + addRow) : visited))
                          obstacles
                  '#' -> move (col, row) restOfDirections nestedArray visited ((col + addCol, row + addRow) : obstacles)
                  f -> error (show f)
                else
                  (visited, reverse obstacles)

move2 :: (Int, Int) -> [(Int, Int)] -> Array Int (Array Int Char) -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> (Int, Int) -> Bool
move2 pos directions nestedArray visited obstacles placed =
  let ((addCol, addRow) : restOfDirections) = directions
   in let (col, row) = pos
       in let char = getInnerValue nestedArray (row + addRow) (col + addCol)
           in if isJust char
                then case fromJust char of
                  inner
                    | (inner == '^' || inner == '.') && (col + addCol, row + addRow) /= placed ->
                        move2
                          (col + addCol, row + addRow)
                          directions
                          nestedArray
                          (if (col + addCol, row + addRow) `elem` visited then visited else ((col + addCol, row + addRow) : visited))
                          obstacles
                          placed
                  blocker | blocker == '#' || (col + addCol, row + addRow) == placed ->
                    if ((addCol, addRow), (col + addCol, row + addRow)) `elem` obstacles then
                      True
                    else
                      move2 (col, row) restOfDirections nestedArray visited (((addCol, addRow), (col + addCol, row + addRow)) : obstacles) placed
                  f -> error (show f)
                else
                  False

main = do
  handle <- openFile "day6.input" ReadMode
  contents <- hGetContents handle
  let directions = cycle [(0, -1), (1, 0), (0, 1), (-1, 0)]
  let nestedArray = buildNestedArray (lines contents)
  let ((r, Just c) : _) =
        filter
          (\(r, x) -> isJust x)
          (map (\(r, a) -> (r, findIndex (\(_, c) -> c == '^') (assocs a))) (assocs nestedArray))
  let (visited, obstacles) = move (c + 1, r) directions nestedArray [] []
  putStrLn (show $ length visited)
  let ans2 = map (\x -> move2 (c + 1, r) directions nestedArray [] [] x) visited
  putStrLn (show $ length (filter (\x -> x == True) ans2))
  hClose handle
