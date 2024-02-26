{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : Weiss Anthony
-}

module LZ.LZW (compress, uncompress) where

import Data.List (elemIndex)
-- import Debug.Trace (trace)
-- | LZW compress method
compress :: String -> [Int]
compress "" = []
compress data' = compress' data' (map (:[]) ['\0'..'\255']) "" []
  where
    compress' [] dict currentCode compressed =
      let index = maybe 0 id (elemIndex currentCode dict)
      in compressed ++ [index]
    compress' (x:xs) dict currentCode compressed =
      let newCode = currentCode ++ [x] in
      case elemIndex newCode dict of
        Just _  -> compress' xs dict newCode compressed
        Nothing -> let index = maybe 0 id (elemIndex currentCode dict)
                       compressed' = compressed ++ [index]
                       dict' = dict ++ [newCode]
                   in compress' xs dict' [x] compressed'

-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress [] = Just ""
uncompress compressedData = uncompress' compressedData (map (:[]) ['\0'..'\255']) ""
  where
    uncompress' [] _ _ = Just []
    uncompress' (x:xs) dict currentCode =
      if x >= 0 && x < length dict then
        let entry = dict !! x
            dict' = if null currentCode then dict else dict ++ [currentCode ++ [head entry]]
        in (entry ++) <$> uncompress' xs dict' entry
      else
        Nothing
