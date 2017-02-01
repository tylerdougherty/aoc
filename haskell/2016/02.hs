-- 3x3 board
isValid3 :: (Int,Int) -> Bool
isValid3 (x,y) = x >= 0 && x <= 2 && y >= 0 && y <= 2

numpad3 = ["123", "456", "789"]

-- 5x5 board
isValid5 :: (Int,Int) -> Bool
isValid5 (x,y) = x >= 0 && x <= 4 && y >= 0 && y <= 4 && (dist (x,y) (2,2)) <= 2

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

numpad5 = ["__1__", "_234_", "56789", "_ABC_", "__D__"]


-- Move in the specified direction if the board allows it
move (x,y) dir isValid
    | dir == 'D' = if isValid (x,y+1) then (x,y+1) else (x,y)
    | dir == 'U' = if isValid (x,y-1) then (x,y-1) else (x,y)
    | dir == 'R' = if isValid (x+1,y) then (x+1,y) else (x,y)
    | dir == 'L' = if isValid (x-1,y) then (x-1,y) else (x,y)

-- From a starting point, run a set of moves and return the ending point
execute :: (Int,Int) -> [Char] -> ((Int,Int) -> Bool) -> (Int,Int)
execute pos [] _ = pos
execute pos (x:xs) isValid = execute (move pos x isValid) xs isValid

-- Aggregate a set of commands recursively, to keep state between executing each command
--findButtons :: (Int,Int) -> [[Char]] -> [Int]
findButtons _ [] _ _ = []
findButtons start (c:cs) board isValid = (getValue result board):(findButtons result cs board isValid)
    where result = execute start c isValid

-- Get the value of the button at a specific position
getValue :: (Int,Int) -> [[Char]] -> Char
getValue (x,y) board = board !! y !! x


run3 = do
    commands <- readFile "2.txt"
    putStrLn $ findButtons (1,1) (words commands) numpad3 isValid3

run5 = do
    commands <- readFile "2.txt"
    putStrLn $ findButtons (0,2) (words commands) numpad5 isValid5
