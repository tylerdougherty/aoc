import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

main :: IO ()
main = do
    file <- readFile "10.txt"
    putStrLn . show $ head . Map.keys . Map.filter f $ runAll (map words $ lines file)
    where f = \a -> sort (values a) == [17, 61]

main2 = do
    file <- readFile "10.txt"
    putStrLn . show $ product . map head . map values . Map.elems . Map.filterWithKey f $ runAll (map words $ lines file)
    where f = \k _ -> k `elem` ["output0","output1","output2"]

runAll = foldr runCommand Map.empty

runCommand :: [String] -> Map String Location -> Map String Location
runCommand ("value":xs) m = reval dest (inter Map.! dest) inter
    where value = Bot [read $ head xs] []
          dest = intercalate "" $ drop 3 xs
          inter = Map.insertWith (<>) dest value m
runCommand ("bot":xs) m = reval source (inter Map.! source) inter
    where source = "bot" ++ head xs
          value = Bot [] dests
          dests = [low, high]
          low = intercalate "" (take 2 $ drop 4 xs)
          high = intercalate "" $ drop 9 xs
          inter = Map.insertWith (<>) source value m

reval :: String -> Location -> Map String Location -> Map String Location
reval k (Bot [v1, v2] [ml, mh]) bots = {-Map.delete k-} rhigh
    where [low,high] = sort [v1,v2]
          withLow = Map.insertWith (<>) ml (Bot [low] []) bots
          rlow = reval ml (withLow Map.! ml) withLow
          withHigh = Map.insertWith (<>) mh (Bot [high] []) rlow
          rhigh = reval mh (withHigh Map.! mh) withHigh
reval _ _ bots = bots


data Location = Bot {values :: ![Int], move :: ![String]}
    deriving (Show)

--Simple union operator
(Bot v1 m1) <> (Bot v2 m2) = Bot (v1++v2) (m1++m2)
