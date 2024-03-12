module Main (main) where

import RLE -- Maintained by Oussama Marwane Hamza
import Statistic.Huffman as Huffman -- Maintained by Nicolas Mendy
import Statistic.ShannonFano as ShannonFano -- Maintained by Marwane Laghzaoui
import LZ.LZ78 as LZ78 -- Maintained by Ethan Pinto
import LZ.LZW as LZW -- Maintained by Weiss Anthony

-- Function to display the compressed and decompressed text based on the method passed as a parameter.
display :: String -> String -> IO ()
display method input = do
  putStrLn $ "\n---------------------\nMethod : " ++ method ++ "\n"
  -- putStrLn $ "Input :\n\n" ++ input ++ "\n"
  case method of
    "RLE" -> do
      let compressed = RLE.compress input
          decompressed = RLE.uncompress compressed
      displayResults compressed decompressed

    "Huffman" -> do
      let (encodingTree, compressed) = Huffman.compress input
          decompressed = Huffman.decompress encodingTree compressed
      displayResults compressed decompressed

    "ShannonFano" -> do
      let (encodingTree, compressed) = ShannonFano.compress input
          decompressed = ShannonFano.uncompress encodingTree compressed
      displayResults compressed decompressed

    "LZ78" -> do
      let compressed = LZ78.compress input
          decompressed = LZ78.uncompress compressed
      displayResults compressed decompressed

    "LZW" -> do
      let compressed = LZW.compress input
          decompressed = LZW.uncompress compressed
      displayResults compressed decompressed

    _ -> putStrLn $ "Unrecognized method : " ++ method ++ "\n"
  where
    displayResults :: Show a => a -> Maybe String -> IO ()
    displayResults compressed decompressed = do
      putStrLn $ "Compressed data:\n\n" ++ show compressed ++ "\n"
      case decompressed of
        Just str -> do
          -- putStrLn $ "Decompressed data:\n\n" ++ str ++ "\n"
          let success = str == input
          putStrLn $ "Lossless decompression: " ++ show success ++ "\n"
        Nothing -> putStrLn "Error during decompression.\n"
      

main :: IO ()
main = do

    -- Demonstration: Compression/Decompression Efficiencies for all methods
  putStrLn "\n\nDemonstration: Compression/Decompression Efficiencies for all methods\n"

  putStrLn "Examples from the PDF\n"

  -- First Example with method RLE
  putStrLn "=====================\nExample 1 : \"aaaabbcbbb\" from slide 15 RLE"
  display "RLE" "aaaabbcbbb"
  display "Huffman" "aaaabbcbbb"
  display "ShannonFano" "aaaabbcbbb"
  display "LZ78" "aaaabbcbbb"
  display "LZW" "aaaabbcbbb"
  -- Second Example with method Huffman & ShannonFano
  putStrLn "=====================\nExample 2 : \"abbca\" from slide 26 Huffman"
  display "RLE" "abbca"
  display "Huffman" "abbca"
  display "ShannonFano" "abbca"
  display "LZ78" "abbca"
  display "LZW" "abbca"
  -- Third Example with method LZ78 & LZW
  putStrLn "=====================\nExample 3 : \"belle echelle !\" from slide 31 LZ78"
  display "RLE" "belle echelle !"
  display "Huffman" "belle echelle !"
  display "ShannonFano" "belle echelle !"
  display "LZ78" "belle echelle !"
  display "LZW" "belle echelle !"

  putStrLn "\nOther examples\n"
  -- Fourth Example with a random binary string
  let example4 = "1101010101101001001010101110110100101011010010111101001010110011101100101111001010101010110111101101011011100100101101010111110101101101110011010111011010101011001101011010100"
  putStrLn $ "=====================\nExample 4 : random binary string : \"" ++ example4 ++ "\""
  display "RLE" example4
  display "Huffman" example4
  display "ShannonFano" example4
  display "LZ78" example4
  display "LZW" example4

  -- Fifth Example with the first paragraph of "Haskell" from wikipedia
  let example5 = "Haskell is a general-purpose, statically-typed, purely functional programming language with type inference and lazy evaluation. Designed for teaching, research, and industrial applications, Haskell has pioneered a number of programming language features such as type classes, which enable type-safe operator overloading, and monadic input/output (IO). It is named after logician Haskell Curry. Haskell's main implementation is the Glasgow Haskell Compiler (GHC). Haskell's semantics are historically based on those of the Miranda programming language, which served to focus the efforts of the initial Haskell working group. The last formal specification of the language was made in July 2010, while the development of GHC continues to expand Haskell via language extensions. Haskell is used in academia and industry. As of May 2021, Haskell was the 28th most popular programming language by Google searches for tutorials, and made up less than 1% of active users on the GitHub source code repository."
  putStrLn $ "=====================\nExample 5 : \"" ++ example5 ++ "\""
  display "RLE" example5
  display "Huffman" example5
  display "ShannonFano" example5
  display "LZ78" example5
  display "LZW" example5