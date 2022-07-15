import Data.Maybe (fromJust)
text = ['0'..'9'] ++ ['A'..'Z']
infinitList = [0..]

base x y | x /= 0 = base (x `div` y) y ++ [fromJust (lookup (x `mod` y) (zip infinitList text))]
         | otherwise = []