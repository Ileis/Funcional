sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' f xs = foldr (\x acc -> f x : acc) [] xs
reverseMap' f xs = foldl (\acc x -> f x : acc) [] xs