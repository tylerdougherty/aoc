run = do
    file <- readFile "3.txt"
    putStrLn $ show $ tris (lines file)


tris :: [[Char]] -> Int
tris [] = 0
tris (st:sts) = result + tris sts
    where result = if validTri (nums st) then 1 else 0
          nums str = map (\a -> read a :: Int) (words str)

validTri [x,y,z] = x < y+z && y < x+z && z < x+y
