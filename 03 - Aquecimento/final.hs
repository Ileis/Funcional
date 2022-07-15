final y [] = []
final y (x:xs) | y == length xs = xs
               | otherwise      = final y xs