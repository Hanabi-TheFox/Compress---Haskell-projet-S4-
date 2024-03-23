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
              -- If there's only one unique symbol, create a leaf node directly
              [(x, _)] -> Just (EncodingLeaf 1 x)
              -- Otherwise, build the Huffman tree
              _ -> Just $ buildHuffmanTree $ orderedCounts input
  where
    orderedCounts :: Ord a => [a] -> [(a, Int)]
    -- Count occurrences of symbols and sort them by frequency
    orderedCounts = sortOn snd . toList . occurrences

    buildHuffmanTree :: [(a, Int)] -> EncodingTree a
    -- Build the Huffman tree from the list of symbol frequencies
    buildHuffmanTree freqs = buildTree $ map (\(x, f) -> EncodingLeaf f x) freqs
      where
        buildTree [t] = t -- If there's only one node, return it as the root of the tree
        -- Otherwise, repeatedly combine nodes until only one remains
        buildTree ts = buildTree $ combineNodes ts
        -- Combine the two lowest frequency nodes
        combineNodes (x:y:xs) = combine x y : combineNodes xs
        combineNodes xs = xs -- If there's only one node remaining, return it as is
        -- Create a parent node with frequency equal to the sum of its children
        combine left right = EncodingNode (frequency left + frequency right) left right

        -- Helper function to get the frequency of a node
        frequency :: EncodingTree a -> Int
        frequency (EncodingLeaf f _) = f
        frequency (EncodingNode f _ _) = f

-- | Compress using Huffman encoding
compress :: Ord a => [a] -> (Maybe (EncodingTree a), [Bit])
compress input =
  case tree input of
    Just encodingTree -> -- If Huffman tree is successfully generated
      -- Encode each symbol in the input using the Huffman tree
      let compressedBits = concatMap (maybe [] id . encode encodingTree) input
      -- Return the Huffman tree along with the compressed bits
      in (Just encodingTree, compressedBits)
    -- If unable to generate Huffman tree, return Nothing along with empty list
    Nothing           -> (Nothing, [])

-- | Decompress using Huffman encoding
decompress :: Maybe (EncodingTree a) -> [Bit] -> Maybe [a]
-- If Huffman tree is provided, decode the compressed bits using it
decompress (Just encodingTree) bits = decode encodingTree bits
-- If Huffman tree is not provided, return Nothing (unable to decompress)
decompress Nothing _              = Nothing
