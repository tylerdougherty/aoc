import Data.Map as Map
import Data.List.Split

getMaxes m = next [] Nothing (Map.toList m)
    where next ks _ [] = ks
          next ks Nothing ((k,v):rest) = next (k:ks) (Just v) rest
          next ks (Just u) ((k,v):rest)
              | v < u = next ks (Just u) rest
              | v > u = next [k] (Just v) rest
              | otherwise = next (k:ks) (Just v) rest
