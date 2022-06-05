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

-- Teste: juntar duas listas, cada elemento xj sera seguido por um elemento yj

juntar [] [] = []
juntar (x:xs) [] = []
juntar [] (y:ys) = []
juntar (x:xs) (y:ys) = x : y : juntar xs ys

list1 = [1..10]
list2 = [x * (-1) | x <- list1]

-- MAP
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

-- FILTER

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x       = x : filter' f xs
                 | otherwise = filter' f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f xs = [x | x <- xs, f x]
