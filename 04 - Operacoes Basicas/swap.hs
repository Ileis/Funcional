swap [] _ _ = []
swap xs y z | y < 0 || y >= (length xs) = xs
            | z < 0 || z >= (length xs) = xs
            | otherwise = primeiro5 particao ++
                          quarto5 particao   ++
                          terceiro5 particao ++
                          segundo5 particao  ++
                          quinto5 particao
            where particao = particionado xs y z

particionado xs y z = (take y xs, [head (drop y xs)] , drop (y + 1) (take z xs), [head (drop z xs)], drop (z + 1) xs)

primeiro5 (x, _, _, _, _) = x
segundo5  (_, x, _, _, _) = x
terceiro5 (_, _, x, _, _) = x
quarto5   (_, _, _, x, _) = x
quinto5   (_, _, _, _, x) = x