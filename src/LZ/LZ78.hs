{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : Ethan Pinto (pintoethan@cy-tech.fr)
-}
module LZ.LZ78(compress, uncompress) where

import Data.Map (Map)
import qualified Data.Map as Map

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress = compress' Map.empty 0
  where
    compress' :: Map String Int -> Int -> String -> [(Int, Char)]
    compress' _ _ [] = []
    compress' dict nextIndex input =
      let (prefix, suffix) = findLongestPrefix dict input
          dict' = Map.insert (prefix ++ [head suffix | not (null suffix)]) (nextIndex + 1) dict
      in (Map.findWithDefault 0 prefix dict, case suffix of { [] -> '\0'; (x:_) -> x }) : compress' dict' (nextIndex + 1) (drop (length prefix + 1) input)

    findLongestPrefix :: Map String Int -> String -> (String, String)
    findLongestPrefix dict input = go [] input
      where
        go prefix [] = (prefix, [])
        go prefix (x:xs) =
          case Map.lookup (prefix ++ [x]) dict of
            Just _ -> go (prefix ++ [x]) xs
            Nothing -> (prefix, x:xs)

-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress = Just . uncompress' []
  where
    uncompress' :: [String] -> [(Int, Char)] -> String
    uncompress' _ [] = []
    uncompress' dict ((0, c) : xs) = c : uncompress' (dict ++ [[c]]) xs
    uncompress' dict ((n, c) : xs) = (dict !! (n - 1) ++ [c]) ++ uncompress' (dict ++ [dict !! (n - 1) ++ [c]]) xs
