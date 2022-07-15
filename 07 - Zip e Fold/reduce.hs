reduce (a, b) = reduce' (a, b) [2..b]

reduce' (a, b) [] = (a, b)
reduce' (a, b) (x:xs) = if ((a `mod` x) == 0 && (b `mod` x) == 0) then reduce' (divT (a, b) x) [2..b `div` x] else reduce' (a, b) xs
divT (a, b) x = (a `div` x, b `div` x)
