import Data.List
import Control.Arrow
import Data.Function

main = do
    file <- readFile "06.txt"
    putStrLn . map mostCommon $ inverse . lines $ file

mostCommon :: String -> Char
mostCommon xs = fst (maximumBy (compare `on` snd) $ count)
    where count = map (head &&& length) (group . sort $ xs)

inverse :: [String] -> [String]
inverse [] = []
inverse xs
    | head xs /= [] = h:t
    | otherwise     = []
    where h = map head xs
          t = inverse $ map tail xs
