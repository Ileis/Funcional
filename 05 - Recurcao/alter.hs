alter n = uniao [x | x <- [1..n]] [-y | y <- [1..n]]

uniao xs ys | xs == [] = []
            | ys == [] = []
            | otherwise = head xs : head ys : uniao (tail xs) (tail ys)