mytails [] = [[]]
mytails xs = xs : mytails (tail xs)