import System.IO

main = do
    handle <- openFile "day1.input" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
