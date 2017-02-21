import Data.List.Split
import Data.List

main = do
    file <- readFile "07.txt"
    putStrLn . show $ countBools (map isTLS $ lines file)

main2 = do
    file <- readFile "07.txt"
    putStrLn . show $ countBools (map isSSL $ lines file)

isSSL s = foldl (||) False $ map (\x -> containsOneOf abas x) odds
    where evens = evenEntries ns
          odds  = oddEntries ns
          ns    = splitOneOf "[]" s
          abas    = foldl (++) [] $ map listFlippedABAs evens

containsOneOf :: [String] -> String -> Bool
containsOneOf xs s = foldl (||) False $ map (\x -> isInfixOf x s) xs

listFlippedABAs :: String -> [String]
listFlippedABAs (a:b:c:xs) = if (a == c && a /= b) then (b:a:b:[]):next else next
    where next = listFlippedABAs (b:c:xs)
listFlippedABAs _ = []

isTLS s = (foldl (||) False evens) && not (foldl (||) False odds)
    where evens = map isABBA (evenEntries ns)
          odds  = map isABBA (oddEntries ns)
          ns    = splitOneOf "[]" s

countBools :: [Bool] -> Int
countBools = foldr ((+) . fromEnum) 0

oddEntries (a:b:xs) = b:(oddEntries xs)
oddEntries _ = []

evenEntries (a:b:xs) = a:(evenEntries xs)
evenEntries (a:xs) = [a]
evenEntries _ = []

isABBA :: String -> Bool
isABBA (a:b:c:d:xs) = (a == d && b == c && a /= b) || isABBA (b:c:d:xs)
isABBA _ = False
