import Data.List
import Control.Applicative

run = do
    file <- readFile "02.txt"
    putStrLn . show . checksum $ lines file

run2 = do
    file <- readFile "02.txt"
    putStrLn . closestMatching $ lines file

checksum :: [String] -> Int
checksum ids = twos * threes
    where twos = length . filter (==True) . map (hasN 2) $ ids
          threes = length . filter (==True) . map (hasN 3) $ ids

hasN :: (Eq a, Ord a) => Int -> [a] -> Bool
hasN n xs = twoMatch . sort $ xs
    where twoMatch (a:as) = (aCount a as) == (n-1) || twoMatch (filter (/=a) as)
          twoMatch _ = False
          aCount a as = length . filter (a==) $ as

cMatch :: (String,String) -> String
cMatch (a,b) = inter a b
    where inter (x:xs) (y:ys)
            | x == y    = x:(inter xs ys)
            | otherwise = inter xs ys
          inter _ _  = []

combos :: [a] -> [(a,a)]
combos (x:xs) = (map ((,) x) xs) ++ combos xs
combos _ = []

closestMatching :: [String] -> String
closestMatching = head . reverse . sortOn length . map cMatch . combos
