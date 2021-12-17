import Data.List ( elemIndex, sort )
import Data.Maybe ( fromJust, mapMaybe, fromMaybe )

run :: IO ()
run = do
    file <- readFile "10.txt"
    let invalidChars = [x | Corrupted x <- map processChunk $ lines file]
    let find = (`lookup` scores1)
    let scores = mapMaybe find invalidChars
    print $ sum scores

run2 :: IO ()
run2 = do
    file <- readFile "10.txt"
    let incompleteChunks = [x | Incomplete x <- map processChunk $ lines file]
    let completions = map (map equivalentClosingChar) incompleteChunks
    let scores = map autocompleteScore completions
    print $ median scores

data BadChunk = Corrupted Char | Incomplete String

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

autocompleteScore :: String -> Int
autocompleteScore = foldl (\acc x -> acc * 5 + find x) 0
    where find a = fromMaybe 0 $ lookup a scores2

openChars :: [Char]
openChars = "([<{"
closeChars :: [Char]
closeChars = ")]>}"
scores1 :: [(Char, Int)]
scores1 = [(')', 3),(']', 57),('}', 1197),('>', 25137)]
scores2 :: [(Char, Int)]
scores2 = [(')', 1),(']', 2),('}', 3),('>', 4)]

processChunk :: String -> BadChunk
processChunk = processChunk' ""

processChunk' :: String -> String -> BadChunk
processChunk' (o:stack) (c:str)
    | c `elem` openChars = processChunk' (c : o : stack) str
    | c == equivalentClosingChar o = processChunk' stack str
    | otherwise = Corrupted c
processChunk' [] (c:str)
    | c `elem` closeChars = Corrupted c
    | otherwise = processChunk' [c] str
processChunk' stack [] = Incomplete stack

-- >>> equivalentClosingChar '['
-- >>> equivalentClosingChar 'v'
-- >>> equivalentClosingChar '}'
-- ']'
-- '_'
-- '_'
equivalentClosingChar :: Char -> Char
equivalentClosingChar o = case elemIndex o openChars of
    Just index -> closeChars !! index
    Nothing -> '_'
