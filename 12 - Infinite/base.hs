import Data.List
import Data.Maybe

text = ['0'..'9'] ++ ['A'..'Z']

base' x y = unfoldr (\b -> if b > 0 then Just(text !! (b `mod` y), b `div` y) else Nothing) x
base x y = reverse $ base' x y