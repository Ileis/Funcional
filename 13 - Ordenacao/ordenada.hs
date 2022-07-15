ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs)
    | x <= y = ordenada xs
    | otherwise = False