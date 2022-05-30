splitints :: (Eq a, Ord a) => (a -> Bool) -> [a] -> ([a], [a])
splitints _ [] = ([],[])
splitints f xs = ([n | n <- xs, f n], [m | m <- xs, not(f m)])