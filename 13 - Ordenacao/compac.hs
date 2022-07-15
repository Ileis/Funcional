import Data.List

compac xs = map (\[a, b] -> if b == 1 then [a] else [a, b]) $ compac' xs

compac' [] = []
compac' l@(x:xs) = [x, repet l] : (compac' . cortaRepet) l

repet [] = 0
repet l@(x:xs) = length $ takeWhile (==x) l

cortaRepet [] = []
cortaRepet (x:xs) = dropWhile (==x) xs