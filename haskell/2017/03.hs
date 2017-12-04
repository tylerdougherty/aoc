import qualified Data.Map.Strict as M

run = do
    putStrLn . show . dist $ 325489

spiral :: [Int]
spiral = [1..] >>= replicate 2

path :: [Char]
path = concat $ zipWith replicate spiral (cycle "RULD")

dist :: Int -> Int
dist n = abs (count 'R' moves - count 'L' moves) + abs (count 'U' moves - count 'D' moves)
    where count i xs = length $ filter (i==) xs
          moves      = take (n-1) path

-- part 2

run2 = do
    putStrLn . show . goUntil $ 325489

trans :: (Int,Int) -> Char -> (Int,Int)
trans (x,y) dir
    | dir == 'R' = (x+1,y)
    | dir == 'U' = (x,y+1)
    | dir == 'L' = (x-1,y)
    | dir == 'D' = (x,y-1)

adjacents :: (Int,Int) -> [(Int,Int)]
adjacents (x,y) = [(i,j) | i <- [x-1 .. x+1], j <- [y-1 .. y+1], i /= x || j /= y]

goUntil n = fill (M.singleton (0,0) 1) (0,0) path
    where
    fill m c (move:ms) = if val > n then val else fill nm nc ms
        where
        nc = trans c move
        val = sum $ map (flip (M.findWithDefault 0) m) $ adjacents nc
        nm = M.insert nc val m
