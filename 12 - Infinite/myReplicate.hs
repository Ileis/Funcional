myreplicate x y | x > 0 = y : myreplicate (x - 1) y
                | otherwise = []