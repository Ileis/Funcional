separa x | x >= 10   = (separa $ x `div` 10) ++ [x `mod` 10]
         | otherwise = [x]