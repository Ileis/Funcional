import Data.Char
import Data.Maybe

alfabetoMin = ['a'..'z']
alfabetoMai = ['A'..'Z']
alfabetoPar = zip alfabetoMin alfabetoMai

upper [] = []
upper (x:xs) | isLower x = fromJust (lookup x alfabetoPar) : upper xs
             | otherwise = x : upper xs