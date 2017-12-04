import Data.List

run = do
    file <- readFile("04.txt")
    putStrLn . show . length . filter validPassphrase $ lines file where
        validPassphrase = hasDuplicates . words

run2 = do
    file <- readFile("04.txt")
    putStrLn . show . length . filter validAnaphrase $ lines file where
        validAnaphrase = hasAnagram . words

hasDuplicates :: [String] -> Bool
hasDuplicates [] = True
hasDuplicates (x:xs) = if elem x xs then False else hasDuplicates xs

hasAnagram :: [String] -> Bool
hasAnagram [] = True
hasAnagram (x:xs) = if anaN x xs then False else hasAnagram xs where
    anaN x xs = or $ map (ana x) xs
    ana a b = elem a $ permutations b
