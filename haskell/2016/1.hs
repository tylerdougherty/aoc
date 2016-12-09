input = "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3"

test = "R5, L5, R5, R3"


--Helpers
killSpace :: [Char] -> [Char]
killSpace (' ':s) = s
killSpace s = s

noComma :: [Char] -> [Char]
noComma "" = ""
noComma (',':xs) = noComma xs
noComma (x:xs) = x:(noComma xs)


data Vec = Vec {x :: Int, y :: Int, dir :: Int} deriving (Show)

pos (Vec x y _) = [x, y]

--turn :: Vec -> Char -> Vec
turn (Vec x y oldDir) 'L' = (Vec x y ((oldDir + 3) `mod` 4))
turn (Vec x y oldDir) 'R' = (Vec x y ((oldDir + 1) `mod` 4))


moves = map noComma (words input)

coords :: Vec -> [[Char]] -> [Int]
coords vec [] = pos vec
coords vec (move:moves) = coords (nextVec vec move) moves


nextVec :: Vec -> [Char] -> Vec
nextVec vec (n:dist) = (move (turn vec n) (read dist :: Int))

move :: Vec -> Int -> Vec
move (Vec x y dir) dist
    | dir == 0 = (Vec x (y+dist) dir)
    | dir == 1 = (Vec (x+dist) y dir)
    | dir == 2 = (Vec x (y-dist) dir)
    | dir == 3 = (Vec (x-dist) y dir)

distance pos = sum (map abs pos)


blocks :: [[Char]] -> Int
blocks moves = 
    let pos = coords (Vec 0 0 0) moves
    in distance pos

