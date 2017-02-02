run = do
    file <- readFile "03.txt"
    putStrLn . show . countTris . traverseh . intify2 $ lines file

run2 = do
    file <- readFile "03.txt"
    putStrLn . show . countTris . traversev . intify2 $ lines file


intify :: [Char] -> [Int]
intify l = map (read :: String -> Int) $ words l

intify2 :: [[Char]] -> [[Int]]
intify2 = map intify

traverseh :: [[Int]] -> [Int]
traverseh = foldr (++) []

traversev :: [[Int]] -> [Int]
traversev xs = (map head xs) ++ rest
    where rest = if done then [] else traversev (map tail xs)
          done = (head $ map tail xs) == []

countTris :: [Int] -> Int
countTris [] = 0
countTris xs = result + (countTris $ drop 3 xs)
    where result = if (validTri $ take 3 xs) then 1 else 0

validTri :: [Int] -> Bool
validTri [x,y,z] = x < y+z && y < x+z && z < x+y
validTri _ = False
