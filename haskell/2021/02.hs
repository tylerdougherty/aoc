run :: IO ()
run = do
    file <- readFile "02.txt"
    let instructions = map parse $ lines file
    print . prod . foldl move (SP 0 0) $ instructions

run2 :: IO ()
run2 = do
    file <- readFile "02.txt"
    let instructions = map parse $ lines file
    print . prod . foldl move (SP2 0 0 0) $ instructions

-- >>> parse "forward 1"
-- ("forward",1)
parse :: [Char] -> (String, Int )
parse s = (dir, read dist)
    where (dir:dist:_) = words s

data SubPosition = SP Int Int | SP2 Int Int Int deriving (Show)

prod :: SubPosition -> Int
prod (SP a b) = a * b
prod (SP2 a b _) = a * b

{-
>>> move (SP 0 0) ("forward", 3)
>>> move (SP 0 0) ("down", 1)
>>> move (SP 10 0) ("up", 5)
SP 0 3
SP 1 0
SP 5 0
>>> move (SP2 0 5 5) ("forward", 8)
SP2 40 13 5
-}
move :: SubPosition -> ([Char], Int) -> SubPosition
move (SP depth hpos) (dir, dist) = case dir of
    "forward" -> SP depth (hpos + dist)
    "down" -> SP (depth + dist) hpos
    "up" -> SP (depth - dist) hpos
    _ -> SP depth hpos
move (SP2 depth hpos aim) (dir, dist) = case dir of
    "forward" -> SP2 (depth + aim * dist) (hpos + dist) aim
    "down" -> SP2 depth hpos (aim + dist)
    "up" -> SP2 depth hpos (aim - dist)
    _ -> SP2 depth hpos aim
