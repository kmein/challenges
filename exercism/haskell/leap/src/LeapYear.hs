module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = year `divisibleBy` 400 || year `divisibleBy` 4 && not (year `divisibleBy` 100)
  where x `divisibleBy` y = x `mod` y == 0
