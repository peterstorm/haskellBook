bloodLevels :: Integer -> String
bloodLevels x
  | x > 145 = show x ++ " is too high!"
  | x < 135 = show x ++ " is too low!"
  | otherwise = show x ++ " is just right."


averageGrade :: (Fractional a, Ord a) => a -> Char
averageGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59  = 'F'
  where y = x / 100
