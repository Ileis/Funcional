intersec [] _ = []
intersec _ [] = []
intersec (x:xs) ys | x `elem` ys = x : intersec xs ys
                   | otherwise   = intersec xs ys