module Main (main) where

import Statistic.Huffman

main :: IO ()
main = do
  let input = "test bola CHIPOLATA 42! [ü]"
      (encodingTree, compressedBits) = compressHuffman input
      decompressedResult = decompressHuffman encodingTree compressedBits

  putStrLn $ "Original Input: " ++ show input
  putStrLn $ "Compressed Bits: " ++ show compressedBits
  putStrLn $ "Decompressed Result: " ++ show decompressedResult
