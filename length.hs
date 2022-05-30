-- opcao 1
length1 [] = 0
length1 xs = sum [1 | _ <- xs]

-- opcao 2

length2 [] = 0
length2 (x:xs) = 1 + length2 xs