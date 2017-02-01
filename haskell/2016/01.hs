input = "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3"

test = "R5, L5, R5, R3"
test2 = "R8, R4, R4, R8"


--Remove commas from the string
noComma :: [Char] -> [Char]
noComma "" = ""
noComma (',':xs) = noComma xs
noComma (x:xs) = x:(noComma xs)

moves = map noComma (words input)
steps = stepify moves


--Convert the 'moves' format (R3, L5, ...) to the 'steps' format (RSSLSRSLLSRSL...)
stepify :: [[Char]] -> [Char]
stepify [] = []
stepify ((dir:dist):moves) = [dir] ++ replicate ((read dist :: Int)-1) 'S' ++ stepify moves


data Vec = Vec {x :: Int, y :: Int, dir :: Int} deriving (Show)
pos (Vec x y _) = [x, y]

--Turn the Vec in a specific direction (L/R/S)
turn :: Vec -> Char -> Vec
turn (Vec x y oldDir) dir
    | dir == 'L' = (Vec x y ((oldDir + 3) `mod` 4))
    | dir == 'R' = (Vec x y ((oldDir + 1) `mod` 4))
    | otherwise  = (Vec x y oldDir)


--Returns the coordinates after executing a set of moves
coords :: Vec -> [Char] -> [[Int]]
coords vec [] = [pos vec]
coords vec (step:steps) =
    let new = (takeStep vec step)
    in [pos new] ++ coords (takeStep vec step) steps

--Move the Vec a distance of one in the target direction
takeStep vec dir = move (turn vec dir) 1

--Move the Vec a specified distance in the target direction
move :: Vec -> Int -> Vec
move (Vec x y dir) dist
    | dir == 0 = (Vec x (y+dist) dir)
    | dir == 1 = (Vec (x+dist) y dir)
    | dir == 2 = (Vec x (y-dist) dir)
    | dir == 3 = (Vec (x-dist) y dir)

--Computes the distance to a coordinate
distance :: [Int] -> Int
distance pos = sum (map abs pos)

--Returns the first duplicate element of the list
firstDupe [] = []
firstDupe (a:as)
    | elem a as = a
    | otherwise = firstDupe as


--Computes the distance to the end of the set of steps
blocks :: [Char] -> Int
blocks steps = 
    let points = coords (Vec 0 0 0) steps
    in distance (last points)

--Finds the first location visited twice along the set of steps
dupe :: [Char] -> Int
dupe steps = distance $ firstDupe $ coords (Vec 0 0 0) steps
