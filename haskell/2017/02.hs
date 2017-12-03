import Data.List

run = do
    file <- readFile("02.txt")
    putStrLn . show . checksum . map (map read) . map words $ lines file

run2 = do
    file <- readFile("02.txt")
    putStrLn . show . divsum . map (map read) . map words $ lines file

divsum :: [[Int]] -> Int
divsum = sum . map divline

divline :: [Int] -> Int
divline xs = head [quot x y | x <- xs, y <- xs, x > y, mod x y == 0]

checksum :: [[Int]] -> Int
checksum = sum . map linesum

linesum :: [Int] -> Int
linesum xs = maximum xs - minimum xs
