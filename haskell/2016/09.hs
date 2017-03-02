import Data.List.Split

main :: IO ()
main = do
    file <- readFile "09.txt"
    putStrLn . show . foldl (+) 0 . map (length . parseFirst) $ lines file

parseFirst :: String -> String
parseFirst [] = ""
parseFirst s
    | a == "" = (concat . replicate reps . take chars $ rp) ++ (parseFirst . drop chars $ rp)
    | otherwise = a ++ parseFirst next
    where a = head $ splitOn "(" s
          next = drop (length a) s
          x = splitOneOf "()" s !! 1
          chars = read $ splitOn "x" x !! 0
          reps = read $ splitOn "x" x !! 1
          lp = head $ splitOn ")" s
          rp = drop (length lp + 1) s
