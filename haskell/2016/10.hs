import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- ### now just add the recursive evaluation :)

main :: IO ()
main = do
    file <- readFile "10.txt"
    putStrLn . show $ foldr (runCommand) Map.empty (map words $ lines file)

runCommand :: [String] -> Map String Location -> Map String Location
runCommand ("value":xs) = Map.insertWith (<>) dest value
    where value = Bot [read $ head xs] []
          dest = intercalate "" $ drop 3 xs
runCommand ("bot":xs) = Map.insertWith (<>) source value
    where source = "bot" ++ head xs
          value = Bot [] dests
          dests = [low, high]
          low = intercalate "" (take 2 $ drop 4 xs)
          high = intercalate "" $ drop 9 xs

reval k (Bot [v1, v2] [ml, mh]) bots = Map.delete k withHigh
    where [low,high] = sort [v1,v2]
          withLow = Map.insertWith (<>) ml (Bot [low] []) bots
          withHigh = Map.insertWith (<>) mh (Bot [high] []) withLow
reval _ _ bots = bots


data Location = Bot {values :: ![Int], move :: ![String]}
    deriving (Show)

--Simple union operator
(Bot v1 m1) <> (Bot v2 m2) = Bot (v1++v2) (m1++m2)

give :: Int -> Location -> Location
give x (Bot vs m) = Bot (x:vs) m

moveCommand :: [String] -> Location -> Location
moveCommand moves (Bot vs _) = Bot vs moves
