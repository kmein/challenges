module Problem45 where

trim :: (Num a,Ord a)
     => (a1 -> a) -> a -> [a1] -> [a]
triangular, pentagonal, hexagonal :: Integral a
                                  => a -> a
isTriangular, isPentagonal, isHexagonal
  :: Integral a
  => a -> Bool
main :: IO ()
trim f n = takeWhile (> n + 1) . dropWhile (< n - 1) . map f

triangular n = n * (n + 1) `div` 2

pentagonal n = n * (3 * n - 1) `div` 2

hexagonal n = n * (2 * n - 1)

isTriangular n = n `elem` triangs
  where triangs = trim triangular n [1 ..]

isPentagonal n = n `elem` pentags
  where pentags = trim pentagonal n [1 ..]

isHexagonal n = n `elem` hexags
  where hexags = trim hexagonal n [1 ..]

main = 
  print . filter (\n -> isHexagonal n && isPentagonal n) . map triangular $
  [(1 :: Integer) ..]
