module Problem6 (square, sumOfSquares, squareOfSum, difference, main) where
---------------------------------------------------------------------------
square :: Int -> Int
square x = x * x

sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map square

squareOfSum :: [Int] -> Int
squareOfSum = square . sum

difference :: [Int] -> Int
difference ns = squareOfSum ns - sumOfSquares ns

main :: IO ()
main = print . difference $ [1..100]
