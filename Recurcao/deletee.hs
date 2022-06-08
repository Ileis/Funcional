deletee :: (Ord a, Eq a) => a -> [a] -> [a]
deletee n [] = []
deletee n all@(x:xs) = if x == n then xs else x : deletee n xs