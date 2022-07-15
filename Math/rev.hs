convertNumList 0 = []
convertNumList x = (x `mod` 10) : convertNumList (x `div` 10)

rev x = foldl (\acc y -> (acc * 10) + y) 0 $ convertNumList x