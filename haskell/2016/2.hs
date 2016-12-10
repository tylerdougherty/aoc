move :: (Int,Int) -> Char -> (Int,Int)
move (x,y) dir
    | dir == 'D' = if y+1 <= 2 then (x,y+1) else (x,2)
    | dir == 'U' = if y-1 >= 0 then (x,y-1) else (x,0)
    | dir == 'R' = if x+1 <= 2 then (x+1,y) else (2,y)
    | dir == 'L' = if x-1 >= 0 then (x-1,y) else (0,y)

execute :: (Int,Int) -> [Char] -> (Int,Int)
execute pos [] = pos
execute pos (x:xs) = execute (move pos x) xs

findButtons :: (Int,Int) -> [[Char]] -> [Int]
findButtons _ [] = []
findButtons start (c:cs) = (posToNum result):(findButtons result cs)
    where result = execute start c

posToNum :: (Int,Int) -> Int
posToNum (x,y) = numpad !! y !! x

numpad = [[1,2,3], [4,5,6], [7,8,9]]

showNums :: [Int] -> [Char]
showNums [] = ""
showNums (z:zs) = (show z)++(showNums zs)


run = do
    commands <- readFile "2.txt"
    putStrLn $ showNums $ findButtons (0,0) (words commands)
