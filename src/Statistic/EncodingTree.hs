{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : Nicolas Mendy (mendynicol@cy-tech.fr)
-}
module Statistic.EncodingTree (
  EncodingTree(..),
  isLeaf,
  count,
  has,
  encode,
  decodeOnce,
  decode,
  meanLength,
  compress,
  uncompress
) where
  
import Statistic.Bit ( Bit(..) )
import Control.Applicative ((<|>))

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ x) y = x == y
has (EncodingNode _ left right) x = has left x || has right x

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ x) y
  | x == y    = Just []
  | otherwise = Nothing
encode (EncodingNode _ left right) x =
  fmap (Zero:) (encode left x) <|> fmap (One:) (encode right x)

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ x) bits = Just (x, bits)
decodeOnce (EncodingNode _ left right) (Zero:rest) = decodeOnce left rest
decodeOnce (EncodingNode _ left right) (One:rest) = decodeOnce right rest
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode _ [] = Just []
decode tree bits = do
  (symbol, rest) <- decodeOnce tree bits
  symbols <- decode tree rest
  return (symbol : symbols)

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = fromIntegral (treeLength tree) / fromIntegral (count tree)

-- Helper function to compute the total length of the tree
treeLength :: EncodingTree a -> Int
treeLength (EncodingLeaf cnt _) = cnt
treeLength (EncodingNode cnt left right) =
  cnt + treeLength left + treeLength right

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress treeGenerator input = case treeGenerator input of
  Just encodingTree -> (Just encodingTree, concatMap (maybe [] id . encode encodingTree) input)
  Nothing           -> (Nothing, [])

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Nothing, _) = Nothing
uncompress (Just tree, bits) = decode tree bits
