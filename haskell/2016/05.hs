import Data.Hash.MD5

main :: IO ()
main = do
    putStrLn ""

--Try using sequence instead of list?
--a = [s | x <- [0..], let s = "abc" ++ show x, let h = hash s, match h]
findMatch s n = if match h then h else findMatch s (n+1)
    where h = md5s $ Str (s ++ show n)

match ('0':'0':'0':'0':'0':_) = True
match _ = False