import qualified Data.Map.Strict as M
import Data.List.Split (splitOneOf)

type Coord = (Int, Int)

run = do
    file <- readFile "03.txt"
    let claims = map parse $ lines file
    let os = overlaps . concat . map inches $ claims
    putStrLn . show . length $ os

run2 = do
    file <- readFile "03.txt"
    let claims = map parse $ lines file
    let os = overlaps . concat . map inches $ claims
    putStrLn . show . Main.id . head . filter (noOverlap os) $ claims

data Claim = Claim { id :: Int
                   , x :: Int
                   , y :: Int
                   , width :: Int
                   , height :: Int
                   } deriving (Show)

parse :: String -> Claim
parse s = claim ns
    where ns = map read . filter (/="") . splitOneOf "#@,:x " $ s
          claim [a,b,c,d,e] = Claim a b c d e

inches :: Claim -> [Coord]
inches (Claim id x y w h) = [(x+wd, y+hd)
                            | wd <- [0..(w-1)]
                            , hd <- [0..(h-1)]
                            ]

overlaps :: [Coord] -> [Coord]
overlaps cs = M.keys . M.filter (>=2) . M.fromListWith (+) . zip cs $ repeat 1

noOverlap :: [Coord] -> Claim  -> Bool
noOverlap coords claim = not . any (`elem` coords) $ is
    where is = inches claim
