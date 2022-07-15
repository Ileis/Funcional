merge [] [] = []
merge xs [] = xs
merge [] as = as
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys