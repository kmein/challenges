module Problem48 where

lastElems :: Int -> [a] -> [a]
lastElems x = reverse . take x . reverse

main :: IO ()
main = print . lastElems 10 . show . sum $ [a ^ a | a <- [1..1000] :: [Integer]]