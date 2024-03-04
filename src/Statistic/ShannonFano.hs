{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : Marwane Laghzaoui (laghzaoui@cy-tech.fr)
-}
module Statistic.ShannonFano (tree,compressShannon,uncompressShannon) where

import Statistic.EncodingTree (EncodingTree(..), encode, decode)
import Statistic.Bit (Bit)
import Statistic.Source (occurrences)
import Data.Map (toList)
import Data.List (sortOn)


-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree input = do
  let counts = orderedCounts input
  if null counts then Nothing else Just (buildTree counts)
  where
    orderedCounts :: Ord a => [a] -> [(a, Int)]
    orderedCounts = sortOn snd . toList . occurrences

-- Helper function to build the Shannon-Fano tree recursively
buildTree :: Ord a => [(a, Int)] -> EncodingTree a
buildTree [(x,occ)] = EncodingLeaf occ x
buildTree txt =  
  let
    totalOccurences = sum (map snd txt)
    (firstHalf, secondHalf) = splitAt (length txt `div` 2) (sortOn snd txt)
   
  in
    EncodingNode totalOccurences (buildTree firstHalf) (buildTree secondHalf)
    
-- Helper function to find the split index in the symbol list
findSplitIndex :: [(a, Int)] -> Int -> Int -> Int -> Int
findSplitIndex ((_, count):rest) currentIndex currentSum targetSum
  | currentSum + count >= targetSum = currentIndex
  | otherwise = findSplitIndex rest (currentIndex + 1) (currentSum + count) targetSum
findSplitIndex _ _ _ _ = error "Invalid split index calculation"

-- | Compress using Shannon-Fano encoding
compressShannon :: Ord a => [a] -> (Maybe (EncodingTree a), [Bit])
compressShannon input =
  case tree input of
    Just encodingTree ->
      let compressedBits = concatMap (\x -> maybe [] id (encode encodingTree x)) input
      in (Just encodingTree, compressedBits)
    Nothing           -> (Nothing, [])

-- | Decompress using Shannon-Fano encoding
uncompressShannon ::  Maybe (EncodingTree a) -> [Bit] -> Maybe [a]
uncompressShannon (Just encodedTree) encodedBits = decode encodedTree encodedBits
uncompressShannon Nothing _ = Nothing