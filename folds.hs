sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' f = foldr (\x acc -> f x : acc) []
reverseMap' f = foldl (\acc x -> f x : acc) []