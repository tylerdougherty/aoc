import Data.List
import Data.List.Split

type Grid = [[Char]]

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop (length xs - n) (cycle xs)) xs

rotateRow :: Int -> Int -> Grid -> Grid
rotateRow i n g = (take i g) ++ [row] ++ (drop (i+1) g)
    where row = rotate n $ g !! i

rotateCol :: Int -> Int -> Grid -> Grid
rotateCol i n g = transpose . rotateRow i n . transpose $ g

rect :: Int -> Int -> Grid -> Grid
rect x y g = map (enable x) (take y g) ++ drop y g

enable n xs = replicate n '#' ++ drop n xs


parseOperation :: String -> Grid -> Grid
parseOperation s
    | op == "rect"   = parseRect s
    | op == "rotate" = parseRotate s
    where ws = words s
          op = ws !! 0

parseRect :: String -> Grid -> Grid
parseRect s = rect a b
    where ws = words s
          ab = splitOn "x" (ws !! 1)
          a  = read $ ab !! 0 :: Int
          b  = read $ ab !! 1 :: Int

parseRotate :: String -> Grid -> Grid
parseRotate s = f a b
    where ws = words s
          f  = if ws !! 1 == "row" then rotateRow else rotateCol
          ab = splitOn "=" s !! 1
          a  = read ab !! 0 :: Int
          b  = read ab !! 2 :: Int



testPrint s = do
    putStrLn . unlines . map show $ s
