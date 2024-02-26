module Main (main) where

import Statistic.Huffman
import LZ.LZW

main :: IO ()
main = do
  let input = "test bola CHIPOLATA 42! [ü]"
      (encodingTree, compressedBits) = compressHuffman input
      decompressedResult = decompressHuffman encodingTree compressedBits

  putStrLn $ "Original Input: " ++ show input
  putStrLn $ "Compressed Bits: " ++ show compressedBits
  putStrLn $ "Decompressed Result: " ++ show decompressedResult

  -- Méthode LZW
  let dataToCompress = "trololo"
  let compressedData = compress dataToCompress
  let decompressedData = uncompress compressedData

  putStrLn $ "Data originale: " ++ dataToCompress
  putStrLn $ "Données compressées: " ++ show compressedData
  case decompressedData of
    Just str -> putStrLn $ "Données décompressées: " ++ str
    Nothing -> putStrLn "Erreur lors de la décompression."
