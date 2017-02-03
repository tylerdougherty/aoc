import qualified Data.Map as Map
import Data.List.Split
import Data.List
import Data.Char

main :: IO ()
main = do
    file <- readFile "04.txt"
    putStrLn . show . validIdSum $ lines file

validIdSum :: [[Char]] -> Int
validIdSum xs = foldr (+) 0 (map validRoom xs)

validRoom :: [Char] -> Int
validRoom r = if isValid then read roomId :: Int else 0
    where [name, roomId, checksum] = [filter isLetter f, filter isNumber f, filter isLetter s]
          [f, s] = splitOn "[" r
          isValid = hasMaxes checksum (letterMap name)

letterMap :: [Char] -> Map.Map Char Int
letterMap = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

hasMaxes :: [Char] -> Map.Map Char Int -> Bool
hasMaxes [] _ = True
hasMaxes (k:ks) m = hasThisMax && hasMaxes ks (Map.delete k m)
    where hasThisMax = k == (head $ sort $ getMaxes m)

getMaxes :: Map.Map Char Int -> [Char]
getMaxes m = next [] Nothing (Map.toList m)
    where next ks _ [] = ks
          next ks Nothing ((k,v):rest) = next (k:ks) (Just v) rest
          next ks (Just u) ((k,v):rest)
              | v < u = next ks (Just u) rest
              | v > u = next [k] (Just v) rest
              | otherwise = next (k:ks) (Just v) rest