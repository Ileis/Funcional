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

-- FOLDL
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- FOLDR

foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- Detalhes da execucao da funcao foldr'.
-- Pontos a serem considerados:
--  - Pilha formada
--  - Recurcao de calda

-- foldr' (+) 0 [1,2,3,4,5] = (+) 1 (foldr' (+) 0 [2,3,4,5])
-- foldr' (+) 0   [2,3,4,5] = (+) 2 (foldr' (+) 0 [3,4,5])
-- foldr' (+) 0     [3,4,5] = (+) 3 (foldr' (+) 0 [4,5])
-- foldr' (+) 0       [4,5] = (+) 4 (foldr' (+) 0 [5])
-- foldr' (+) 0         [5] = (+) 5 (foldr' (+) 0 [])
-- foldr' (+) 0          [] = 0
-- foldr' (+) 0         [5] = (+) 5 0 = 5
-- foldr' (+) 0       [4,5] = (+) 4 5 = 9
-- foldr' (+) 0     [3,4,5] = (+) 3 9 = 12
-- foldr' (+) 0   [2,3,4,5] = (+) 2 12 = 14
-- foldr' (+) 0 [1,2,3,4,5] = (+) 1 14 = 15





elem' x = foldl' (\acc y -> if y == x then True else acc) False