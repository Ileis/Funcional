import Data.Char
import Data.List
import Data.Maybe

convertStringTupla xs = map (\string -> (convertStringToInt $ words string !! 0,convertStringToInt $ words string !! 1)) xs

convertStringToInt xs = map digitToInt xs

convertListToInt :: [Int] -> Int
convertListToInt [] = 0
convertListToInt l@(x:xs) = (x * 10 ^ length xs) + convertListToInt xs

convertTuplaListToNum xs = map (\(a, b) -> (convertListToInt a, convertListToInt b)) xs

stringToTuplaNum xs =convertTuplaListToNum $ convertStringTupla xs

tirarInvalidos xs = map (\(a, b) -> if (a < 10 || b < 10) then Nothing else Just (a, b)) xs

calcDist xs = map (\par -> if isNothing par then Nothing else Just (abs((fst $ fromJust par) - (snd $ fromJust par)))) xs

distList xs = calcDist $ tirarInvalidos $ stringToTuplaNum xs

tirarNothing = filter isJust

processa xs 
    | null $ tirarNothing $ distList xs = "sem ganhador"
    | otherwise = show $ fromJust $ elemIndex (minimum $ tirarNothing $ distList xs) $ distList xs

-- processa ["8 11", "10 15"] == "1"
-- processa ["9 12", "11 13", "10 11"] == "2"
-- processa ["12 15", "16 14", "10 9"] == "1"
-- processa ["12 15", "20 23", "10 9", "35 35"] == "3"
-- processa ["10 8", "9 13"] == "sem ganhador"
-- processa ["8 9", "12 7"] == "sem ganhador"
-- processa ["10 9", "15 19"] == "1"
-- processa ["9 8", "9 12", "12 15", "18 19"] == "3"