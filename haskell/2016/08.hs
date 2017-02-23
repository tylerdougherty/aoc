import Data.List

type Grid = [[Char]]

rotate n xs = zipWith const (drop (length xs - n) (cycle xs)) xs

--splitAt?
--take i xs, drop (i+1) xs
rotateRow i n xs = rotate n $ xs !! i

rotateCol i n xs = transpose . rotate n . transpose $ xs !! i