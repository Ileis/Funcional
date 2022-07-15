quadperf = temRaizInteira 1

temRaizInteira n x | (n * n) < x  = temRaizInteira (n + 1) x
                   | (n * n) == x = True
                   | otherwise    = False