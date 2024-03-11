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
import Data.List (sortOn,minimumBy)
import Data.Ord (comparing)
import Control.Arrow ()

-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree input = do
  let counts = orderedCounts input
  if null counts then Nothing else Just (buildTree counts)
  where
    orderedCounts :: Ord a => [a] -> [(a, Int)]
    orderedCounts = sortOn snd . toList . occurrences

buildTree :: Ord a => [(a, Int)] -> EncodingTree a
buildTree [(x,occ)] = EncodingLeaf occ x
buildTree txt =  
  let
    totalOccurences = sum (map snd txt)
    (firstHalf, secondHalf) = splitAt (splitIndex txt) (sortOn snd txt) -- length txt `div` 2
   
  in
    EncodingNode totalOccurences (buildTree firstHalf) (buildTree secondHalf)
    
splitIndex :: [(a, Int)] -> Int
splitIndex txt
  | length txt < 2 = 0 -- Making sure we use an array
  | otherwise =
    let
      index = [1..length txt - 1]
      differencesArray = map (calculateDifference txt) index
      minIndex = findSplitIndex differencesArray
    in
      minIndex

-- | Sums the occurences of the left part and right part given an index, then calculates the differences
calculateDifference :: [(a, Int)] -> Int -> Int
calculateDifference txt index =
  let
    (left, right) = splitAt index txt
    leftCount = sum (map snd left)
    rightCount = sum (map snd right)
  in
    abs (leftCount - rightCount)

-- | Returns the index of the minimum occurences difference between two different parts of the array
findSplitIndex :: Ord a => [a] -> Int
findSplitIndex tab = snd (minimumBy (comparing fst) (zip tab [1..]))

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