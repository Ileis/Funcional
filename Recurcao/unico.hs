-- opcao 1

unico1 y xs | freq y xs == 1 = True
            | otherwise = False

freq y [] = 0
freq y (x:xs) | y == x = 1 + freq y xs
              | otherwise = freq y xs

-- opcao 2

unico2 x xs = if length (filter (==x) xs) == 1 then True else False