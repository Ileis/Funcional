-- opcao 1

somaImpares xs = sum (filter odd xs)

-- opcao 2

somaImpares' xs = sum [x | x <- xs, odd x]