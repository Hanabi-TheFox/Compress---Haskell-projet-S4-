module Main (main) where

import Statistic.Huffman
import LZ.LZW
import LZ.LZ78
import RLE

main :: IO ()
main = do
  -- Méthode Huffman
  putStrLn "Methods : Huffman\n"
  let input = "test bola CHIPOLATA 42! [ü]"
      (encodingTree, compressedBits) = compressHuffman input
      decompressedResult = decompressHuffman encodingTree compressedBits

  putStrLn $ "Original Input: " ++ show input
  putStrLn $ "Compressed Bits: " ++ show compressedBits
  putStrLn $ "Decompressed Result: " ++ show decompressedResult

  -- Méthode LZW
  putStrLn "\n\nMethods : LZW\n"
  let dataToCompress = "trololo"
  let compressedData = LZ.LZW.compress dataToCompress
  let decompressedData = LZ.LZW.uncompress compressedData

  putStrLn $ "Data originale: " ++ dataToCompress
  putStrLn $ "Données compressées: " ++ show compressedData
  case decompressedData of
    Just str -> putStrLn $ "Données décompressées: " ++ str
    Nothing -> putStrLn "Erreur lors de la décompression."

   -- test for the LZ78
  let inputString = "belle echelle !"
      compressed = LZ.LZ78.compress inputString
      decompressed = LZ.LZ78.uncompress compressed

  putStrLn "\n\nMethods : LZ78\n"
  putStrLn "Input String:"
  putStrLn inputString

  putStrLn "\nCompressed Codes:"
  print compressed

  putStrLn "\nDecompressed Result:"
  case decompressed of
    Just result -> putStrLn result
    Nothing -> putStrLn "Unable to decompress the input"
  
  -- Méthode RLE
  putStrLn "\n\nMethods : RLE\n"
  let rleInput = "oussamamarwwwanehamza"
      rleCompressed = RLE.compress rleInput
      rleDecompressed = RLE.uncompress rleCompressed 

  putStrLn $ "Data originale: " ++ rleInput
  putStrLn $ "Données compressées: " ++ show rleCompressed
  case rleDecompressed of
    Just str -> putStrLn $ "Données décompressées: " ++ str
    Nothing -> putStrLn "Erreur lors de la décompression."