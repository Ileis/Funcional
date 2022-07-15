import Data.List

qsort [] = []
qsort l@(x:xs) = qsort menor ++ [x] ++ qsort maior
            where menorMaior = partition (< x) l
                  menor = fst menorMaior
                  maior = tail $ snd menorMaior