module Problem21 (divisors, d, amicable) where
-----------------------------------------------
divisors :: Integer -> [Integer]
divisors n = filter  ((== 0) . mod n) [1..(n-1)]

d :: Integer -> Integer
d = sum . divisors

amicable :: Integer -> Integer -> Bool
amicable a b = (d a == b) && (d b == a) && (a /= b)

-- TODO