{-# LANGUAGE FlexibleInstances #-}
module DictionaryCompression
    ( sharedPrefix
    , compress
    ) where

data Compressed a = Compressed
    { word :: a
    , prefix :: Int
    }

instance Show (Compressed String) where
    show c
        | prefix c > 0 = show (prefix c) ++ drop (prefix c) (word c)
        | otherwise = word c

-- | Returns the length of the shared prefix of two lists.
-- >>> sharedPrefix "an" "ant"
-- 2
sharedPrefix :: Eq a => [a] -> [a] -> Int
sharedPrefix xs ys = length . takeWhile (uncurry (==)) $ zip xs ys

extend :: Eq a => Maybe [a] -> [a] -> Compressed [a]
extend prev xs =
    Compressed
    { word = xs
    , prefix = maybe 0 (sharedPrefix xs) prev
    }

-- | >>> compress ["cell","cello","cellos"]
-- [cell,4o,5s]
compress :: Eq a => [[a]] -> [Compressed [a]]
compress = extendList Nothing
  where
    extendList p [x] = [extend p x]
    extendList p (x:xs) = extend p x : extendList (Just x) xs
