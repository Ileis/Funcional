-- opcao 1

frequencia1 x xs = length (filter (==x) xs)

-- opcao 2

frequencia2 x xs = length [y | y <- xs, y == x]

-- opcao 3

frequencia3 x [] = 0
frequencia3 x (y:ys) | x == y = 1 + frequencia3 x ys
                     | otherwise = frequencia3 x ys