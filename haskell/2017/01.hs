import Data.Char

run = do
    file <- readFile "01.txt"
    putStrLn . show . sumMatches 1 . map digitToInt . head $ lines file

run2 = do
    file <- readFile "01.txt"
    let xs = head $ lines file
    putStrLn . show . sumMatches (quot (length xs) 2) . map digitToInt $ xs

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

sumMatches :: Int -> [Int] -> Int
sumMatches n xs = sum . zipWith match xs $ rotate n xs

match :: Int -> Int -> Int
match x y = if x == y then x else 0
