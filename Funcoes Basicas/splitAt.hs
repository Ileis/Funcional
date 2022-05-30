divide [] _ = ([],[])
divide (x:xs) y | y == 0 = ([], x:xs)
                | otherwise = ( x : fst(divide xs (y - 1)), snd(divide xs (y - 1)))