run :: IO ()
run = do
    file <- readFile "01.txt"
    print . length . filter (> 0) . deltas . map read $ lines file

run2 :: IO ()
run2 = do
    file <- readFile "01.txt"
    print . length . filter (> 0) . deltas . threes . map read $ lines file

-- >>> threes [199,200,208,210,200,207,240,269,260,263]
-- [607,618,618,617,647,716,769,792]
threes :: Num a => [a] -> [a]
threes (a:b:c:xs) = sum [a,b,c] : threes (b:c:xs)
threes _ = []

-- >>> deltas [199,200,208,210,200,207,240,269,260,263]
-- [1,8,2,-10,7,33,29,-9,3]
deltas :: Num a => [a] -> [a]
deltas (a:b:xs) = (b - a) : deltas (b:xs)
deltas _ = []
