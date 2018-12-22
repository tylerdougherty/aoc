import Data.Char
import qualified Data.Set as Set

run = do
    file <- readFile "01.txt"
    putStrLn . show . sum . map toNum $ lines file

run2 = do
    file <- readFile "01.txt"
    let freqs = scanl (+) 0 . cycle . map toNum $ lines file
    putStrLn . show . firstDuplicate $ freqs

firstDuplicate :: (Eq a, Ord a) => [a] -> a
firstDuplicate xs = dup Set.empty xs
    where dup seen (n:ns)
            | n `Set.member` seen = n
            | otherwise = dup (Set.insert n seen) ns

toNum :: [Char] -> Int
toNum ('+':n) = read n
toNum ('-':n) = negate $ read n
