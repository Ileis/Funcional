-- opcao 1

pertence1 y xs = elem y xs

-- opcao 2

pertence2 y xs = not (null [x | x <- xs, x == y])

-- opcao 3

pertence3 y [] = False
pertence3 y (x:xs) | y == x = True
                   | otherwise = pertence3 y xs
