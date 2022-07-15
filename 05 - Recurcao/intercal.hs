intercal xs ys | xs == []  = ys
               | ys == []  = xs
               | otherwise = head xs : head ys : intercal (tail xs) (tail ys)

-- com zip e fold

intercal1 :: (Num a, Eq a) => [a] -> [a] -> [a]
intercal1 xs [] = xs
intercal1 [] ys = ys
intercal1 xs ys | length xs < length ys = foldr (\(a, b) acc -> if a /= -1 then a : b : acc else b : acc) [] $ zipado xs ys
                | otherwise             = foldr (\(a, b) acc -> if b /= -1 then a : b : acc else a : acc) [] $ zipado xs ys

zipado :: (Num a, Num b) => [a] -> [b] -> [(a, b)]
zipado xs ys | length xs < length ys = zip (xs ++ repeat (-1)) ys
             | length ys < length xs = zip xs (ys ++ repeat (-1))
             | otherwise             = zip xs ys

-- intercal1 [1,2,3] [7,8,9] == [1,7,2,8,3,9]
-- intercal1 [1,2,3,4] [8,9] == [1,8,2,9,3,4]
-- intercal1 [5] [1,2,6] == [5,1,2,6]