import System.IO
import Data.List

readN :: String -> Int
readN n = read n

tuple :: [String] -> (Int, Int)
tuple (h1:h2:_) = (read h1,read h2)

absDiff :: (Int, Int) -> Int
absDiff (one,two) = abs (one - two)

count :: [Int] -> Int -> Int
count lst n = length $ filter (\x -> x == n) lst

main = do
    handle <- openFile "day1.input" ReadMode
    contents <- hGetContents handle
    let split = lines contents
    let pairs = map words split
    let (first, second) = unzip $ map tuple pairs
    let first_sorted = sort first
    let second_sorted = sort second
    let zipped = zip first_sorted second_sorted
    let ans = foldr (\x acc -> acc + absDiff x) 0 zipped
    putStrLn (show ans)
    let ans2 = foldr (\x acc -> acc + x) 0 (map (\n -> n * count second n) first)
    putStrLn (show ans2)
    hClose handle
