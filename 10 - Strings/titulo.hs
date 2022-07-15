import Data.Char
import Data.List

titulo xs = unwords $ map convertListToTitle $ words xs

convertListToTitle [] = []
convertListToTitle (x:xs) = toUpper x : convertListToLower xs

convertListToLower [] = []
convertListToLower xs = map toLower xs