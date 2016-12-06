findFloor :: [Char] -> Integer
findFloor ('(':str) = 1 + findFloor(str)
findFloor (')':str) = findFloor(str) - 1
findFloor _ = 0
