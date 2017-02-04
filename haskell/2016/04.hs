import qualified Data.Map as Map
import Data.List.Split
import Data.List
import Data.Char

main :: IO ()
main = do
    file <- readFile "04.txt"
    putStrLn . show . validIdSum $ lines file
    putStrLn . show . validRoomNames $ lines file

validIdSum :: [[Char]] -> Int
validIdSum xs = foldr (+) 0 (map validRoom xs)

validRoomNames :: [[Char]] -> [[Char]]
validRoomNames xs = map decode names
    where validRooms = filter (\x -> validRoom x > 0) xs
          names = map (head . splitOn "[") validRooms

decode :: [Char] -> [Char]
decode s = map (shiftLower (roomId s)) s

shiftLower :: Int -> Char -> Char
shiftLower n c
    | c == '-'  = ' '
    | isLower c = chr $ mod (ord c - ord 'a' + n) 26 + ord 'a'
    | otherwise = c

roomId :: [Char] -> Int
roomId = read . filter isNumber

validRoom :: [Char] -> Int
validRoom r = if isValid then roomId f else 0
    where [name, checksum] = [filter isLetter f, filter isLetter s]
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