{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : Nicolas Mendy (mendynicol@cy-tech.fr)
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import Data.Map (Map, fromListWith, toList)
import Data.List (sortOn)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences = fromListWith (+) . map (\x -> (x, 1))

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy input = sum [- p * logBase 2 p | p <- probabilities]
  where
    counts = occurrences input
    totalCount = fromIntegral (length input)
    probabilities = map (\(_, cnt) -> fromIntegral cnt / totalCount) (toList counts)

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts = sortOn snd . toList . occurrences
