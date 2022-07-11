import Data.Maybe (fromJust)
alfabetoNumerado = zip ['A'..'Z'] [0..]

numerarFrase = map (\x ->(x, fromJust $ lookup x alfabetoNumerado))

combinarChaves xs ys = zip (numerarFrase xs) (cycle (numerarFrase ys))

criptografar xs ys = map (\((a, b), (c, d)) -> (b + d) `mod` 26) $ combinarChaves xs ys

vigenere xs ys = [fromJust $ lookup x (map (\(a, b) -> (b, a)) alfabetoNumerado) | x <- criptografar xs ys]