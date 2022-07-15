ehPrimo x = divisores <= 2
           where divisores = length [x `mod` y | y <- [1..x], x `mod` y == 0]