import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed((!), Vector, (//))

run = do
    file <- readFile("05.txt")
    putStrLn . show . length . maze 0 . V.fromList . map read $ lines file

run2 = do
    file <- readFile("05.txt")
    putStrLn . show . length . maze2 0 . V.fromList . map read $ lines file

maze :: Int -> Vector Int -> [Int]
maze i xs
    | i < 0 || i >= V.length xs = []
    | otherwise = c:(maze (i+c) nxs) where
    c = xs ! i
    nxs = xs // [(i, c+1)]

maze2 :: Int -> Vector Int -> [Int]
maze2 i xs
    | i < 0 || i >= V.length xs = []
    | otherwise = c:(maze2 (i+c) nxs) where
    c = xs ! i
    nxs = xs // [(i, if c >=3 then c-1 else c+1)]
