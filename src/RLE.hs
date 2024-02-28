{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : Oussama EL FEHRI oussamaelfehri712@gmail.com
-}
module RLE(compress, uncompress) where
   
-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compress' xs x 1
  where
    compress' :: Eq a => [a] -> a -> Int -> [(a, Int)]
    compress' [] sym count = [(sym, count)]
    compress' (y:ys) sym count
      | y == sym  = compress' ys sym (count + 1)
      | otherwise = (sym, count) : compress' ys y 1

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress ((sym, count):rest)
  | count < 1  = Nothing
  | otherwise = case uncompress rest of
                  Just uncompressed -> Just (replicate count sym ++ uncompressed)
                  Nothing           -> Nothing