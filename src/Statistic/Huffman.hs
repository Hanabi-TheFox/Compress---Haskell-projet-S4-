{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : Nicolas Mendy (mendynicol@cy-tech.fr)
-}
module Statistic.Huffman (tree, compress, decompress) where

import Data.List (sortOn)
import Data.Map (toList)
import Statistic.EncodingTree (EncodingTree(..), encode, decode)
import Statistic.Bit (Bit)
import Statistic.Source (occurrences)

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree input = case orderedCounts input of
               [] -> Nothing
               [(x, _)] -> Just (EncodingLeaf 1 x)
               _ -> Just $ buildHuffmanTree $ orderedCounts input
  where
    orderedCounts :: Ord a => [a] -> [(a, Int)]
    orderedCounts = sortOn snd . toList . occurrences

    buildHuffmanTree :: [(a, Int)] -> EncodingTree a
    buildHuffmanTree freqs = buildTree $ map (\(x, f) -> EncodingLeaf f x) freqs
      where
        buildTree [t] = t
        buildTree ts = buildTree $ combineNodes ts
        combineNodes (x:y:xs) = combine x y : combineNodes xs
        combineNodes xs = xs
        combine left right = EncodingNode (frequency left + frequency right) left right

        frequency :: EncodingTree a -> Int
        frequency (EncodingLeaf f _) = f
        frequency (EncodingNode f _ _) = f

-- | Compress using Huffman encoding
compress :: Ord a => [a] -> (Maybe (EncodingTree a), [Bit])
compress input =
  case tree input of
    Just encodingTree ->
      let compressedBits = concatMap (maybe [] id . encode encodingTree) input
      in (Just encodingTree, compressedBits)
    Nothing           -> (Nothing, [])

-- | Decompress using Huffman encoding
decompress :: Maybe (EncodingTree a) -> [Bit] -> Maybe [a]
decompress (Just encodingTree) bits = decode encodingTree bits
decompress Nothing _              = Nothing
