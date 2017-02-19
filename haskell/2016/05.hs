import Data.Hash.MD5
import Data.Char

main :: IO ()
main = do
    putStrLn $ take 8 ((!! 5) <$> matches "abbhdwsy")

main2 :: IO ()
main2 = do
    putStrLn $ fillPass emptyPass goodPairs
    where emptyPass = replicate 8 '-'
          pairs     = (\s -> (digitToInt (s !! 5), s !! 6)) <$> matches "abbhdwsy"
          goodPairs = filter ((<8) . fst) pairs

hashes :: String -> [String]
hashes s = md5ify . (s ++) . show <$> [(0 :: Integer)..]

md5ify :: String -> String
md5ify s = md5s $ Str s

match :: String -> Bool
match s = take 5 s == "00000"

matches :: String -> [String]
matches s = filter match (hashes s)

replaceAt :: Int -> Char -> String -> String
replaceAt n x ys = fst split ++ x : (tail $ snd split)
    where split = splitAt n ys

tryReplace :: Int -> Char -> String -> String
tryReplace n x ys
    | ys !! n == '-' = replaceAt n x ys
    | otherwise = ys

fillPass :: String -> [(Int, Char)] -> String
fillPass s xs
    | count == (length s) = s
    | otherwise = fillPass new (tail xs)
    where count = length $ filter (/='-') s
          new   = tryReplace (fst item) (snd item) s
          item  = head xs
