expoentes x y =  invert (-) 1 . length $ takeWhile (\b -> snd b == 0) $ iterate (\par -> fst par `divMod` y) (x, 0)

invert :: (a -> b -> c) -> (b -> a -> c)
invert f x y = f y x