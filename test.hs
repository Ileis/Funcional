maximum' [] = error "lista vazia"
maximum' [x] = x
maximum' (x:xs) | x > maximum xs = x
                | otherwise = maximum xs

replicate' y x | y == 0 = []
               | otherwise = x : replicate' x (y - 1)
take' _ [] = []
take' y (x:xs) | y <= 0    = []
               | otherwise = x : take' (y - 1) xs

reverse' [] = []
reverse' (x:xs) = append' x (reverse' xs)

append' y [] = [y]
append' y (x:xs) = x : append' y xs

repeat' x = x : repeat' x

quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
                   where smallerSorted = quicksort [w | w <- xs, w <= x]
                         biggerSorted  = quicksort [y | y <- xs, y >  x]