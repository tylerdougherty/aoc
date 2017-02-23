type Grid = [[Char]]

rotate n xs = zipWith const (drop (length xs - n) (cycle xs)) xs
