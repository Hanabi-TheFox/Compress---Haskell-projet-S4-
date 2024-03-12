{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : Nicolas Mendy (mendynicol@cy-tech.fr)
-}
module Statistic.Huffman (tree, compress, decompress) where

import Statistic.EncodingTree ( EncodingTree(..), encode, decode )
import Statistic.Bit ( Bit )
import Statistic.Source (occurrences)
import Data.List (foldl', sortOn)
import Data.Map (toList)

buildHuffmanTree :: [(a, Int)] -> EncodingTree a
buildHuffmanTree [] = EncodingLeaf 0 undefined
buildHuffmanTree [(x, _)] = EncodingLeaf 1 x
buildHuffmanTree counts = foldl' buildLeaf (buildInitialTree counts) (sortOn snd counts)
  where
    buildInitialTree = foldl' (\acc (x, cnt) -> EncodingNode cnt (EncodingLeaf cnt x) acc) (EncodingLeaf 0 undefined)
    buildLeaf accTree (x, cnt) = EncodingNode cnt (EncodingLeaf cnt x) accTree

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree input = do
  let counts = orderedCounts input
  if null counts then Nothing else Just (buildHuffmanTree counts)
  where
    orderedCounts :: Ord a => [a] -> [(a, Int)]
    orderedCounts = sortOn snd . toList . occurrences

-- | Compress using Huffman encoding
compress :: Ord a => [a] -> (Maybe (EncodingTree a), [Bit])
compress input =
  case tree input of
    Just encodingTree ->
      let compressedBits = concatMap (\x -> maybe [] id (encode encodingTree x)) input
      in (Just encodingTree, compressedBits)
    Nothing           -> (Nothing, [])

-- | Decompress using Huffman encoding
decompress :: Maybe (EncodingTree a) -> [Bit] -> Maybe [a]
decompress (Just encodingTree) bits = decode encodingTree bits
decompress Nothing _              = Nothing
