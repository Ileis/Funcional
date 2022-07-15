myelemIndex y xs = myelemindex' y xs 0

myelemindex' y [] i = Nothing
myelemindex' y (x:xs) i | y == x = Just i
                        | otherwise = myelemindex' y xs (i + 1)