sublist = sublist'

sublist' x y zs | begin < end = zs !! index begin : sublist' (begin + 1) y zs
                | begin == end = []
                | otherwise = sublist' (begin + 1) y zs
                 where begin = if x < 0 then x `mod` tam else x
                       end   = if y < 0 then y `mod` tam else y
                       tam   = length zs
                       index j = j `mod` tam

-- sublist 1 3 [0,1,2,3,4,5,6,7,8,9,10] == [1,2]
-- sublist 0 11 [0,1,2,3,4,5,6,7,8,9,10] == [0,1,2,3,4,5,6,7,8,9,10]
-- sublist 2 8 [0,1,2,3,4,5,6,7,8,9,10] == [2,3,4,5,6,7]
-- sublist 0 (-1) [0,1,2,3,4,5,6,7,8,9,10] == [0,1,2,3,4,5,6,7,8,9]
-- sublist 2 (-2) [0,1,2,3,4,5,6,7,8,9,10] == [2,3,4,5,6,7,8]
-- sublist (-10) (-1) [0,1,2,3,4,5,6,7,8,9,10] == [1,2,3,4,5,6,7,8,9]
-- sublist (-4) (-2) [0,1,2,3,4,5] == [2,3]