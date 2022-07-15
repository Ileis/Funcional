import Data.Maybe

somaMaybe x y | isNothing x && isNothing y = Nothing
              | isNothing x = y
              | isNothing y = x
              | otherwise = Just(fromJust x + fromJust y)

-- somaMaybe (Just 5) (Just 7) == (Just 12)
-- somaMaybe (Just 5) Nothing == (Just 5)
-- somaMaybe Nothing (Just 3) == (Just 3)
-- somaMaybe Nothing Nothing == Nothing