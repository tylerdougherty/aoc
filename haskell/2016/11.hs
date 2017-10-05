import Data.List

pairs :: [String] -> [[String]]
pairs = removeDoubles . removeDuplicates . allPairs
    where removeDoubles = filter ((1/=) . length . nub)
          removeDuplicates = nub . map sort
          allPairs as = mapM (const as) [1..2]

deleteAll :: [String] -> [String] -> [String]
deleteAll xs a = foldr1 intersect $ sequence (map delete xs) a
