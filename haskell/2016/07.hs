import Data.List.Split

main = do
    file <- readFile "07.hs"
    putStrLn . show $ countBools (map isTLS $ lines file)

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
