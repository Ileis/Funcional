import Distribution.Simple.Utils (xargs)
elemento y [] = error "lista vazia"
elemento y xs = xs !! index
                where index = y `mod` length xs

elemento' y [] = error "lista vazia"
elemento' y all@(x:xs) | index == 0 = x
                       | otherwise = elemento' (index - 1) xs
                       where index = y `mod` length all