import Data.List.Split
import Data.List.Extra (stripInfix)
import Data.Maybe

main :: IO ()
main = do
    file <- readFile "09.txt"
    putStrLn . show . foldl (+) 0 . map (length . parseFirst) $ lines file

main2 :: IO ()
main2 = do
    file <- readFile "09.txt"
    putStrLn . show . foldl (+) 0 . map count . lines $ file

splitOnOne :: String -> String -> (String, String)
splitOnOne x ys = fromJust $ stripInfix x ys

parseFirst :: String -> String
parseFirst [] = ""
parseFirst s
    | a == "" = (concat . replicate reps . take chars $ rp) ++ (parseFirst . drop chars $ rp)
    | otherwise = a ++ parseFirst next
    where a = head $ splitOn "(" s
          next = drop (length a) s
          x = splitOneOf "()" s !! 1
          [chars, reps] = map read $ splitOn "x" x
          lp = head $ splitOn ")" s
          rp = drop (length lp + 1) s

count :: String -> Int
count ('(':xs) =
    let (nums, rest) = splitOnOne ")" xs
        [chars, reps] = map read $ splitOn "x" nums
        str = take chars rest
    in (reps * count str) + count (drop chars rest)
count (x:xs) = 1 + count xs
count [] = 0
