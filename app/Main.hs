module Main (main) where

import RLE -- Create by Oussama Marwane Hamza
import Statistic.Huffman as Huffman -- Create by Nicolas Mendy
import Statistic.ShannonFano as ShannonFano -- Create by Marwane Laghzaoui
import LZ.LZ78 as LZ78 -- Create by Ethan Pinto
import LZ.LZW as LZW -- Create by Weiss Anthony


-- Fonction pour afficher le texte compressé et décompressé selon la méthode passé en paramètre.
afficher :: String -> String -> IO ()
afficher methode input = do
  putStrLn $ "\n\nMéthode : " ++ methode ++ "\n"
  putStrLn $ "Input : " ++ input
  case methode of
    "RLE" -> do
      let compressed = RLE.compress input
          decompressed = RLE.uncompress compressed
      afficherResultats compressed decompressed

    "Huffman" -> do
      let (encodingTree, compressed) = Huffman.compress input
          decompressed = Huffman.decompress encodingTree compressed
      afficherResultats compressed decompressed

    "ShannonFano" -> do
      let (tree, compressed) = compressShannon input
          decompressed = uncompressShannon tree compressed
      afficherResultats compressed decompressed

    "LZ78" -> do
      let compressed = LZ78.compress input
          decompressed = LZ78.uncompress compressed
      afficherResultats compressed decompressed

    "LZW" -> do
      let compressed = LZW.compress input
          decompressed = LZW.uncompress compressed
      afficherResultats compressed decompressed

    _ -> putStrLn $ "Méthode non reconnue : " ++ methode
  where
    afficherResultats :: Show a => a -> Maybe String -> IO ()
    afficherResultats compressed decompressed = do
      putStrLn $ "Données compressées: " ++ show compressed
      case decompressed of
        Just str -> putStrLn $ "Données décompressées: " ++ str
        Nothing -> putStrLn "Erreur lors de la décompression."

main :: IO ()
main = do

  -- -- Méthode RLE
  -- putStrLn "\n\nMethods : RLE\n"
  -- let rleInput = "oussamamarwwwanehamza"
  --     rleCompressed = RLE.compress rleInput
  --     rleDecompressed = RLE.uncompress rleCompressed 

  -- putStrLn $ "Data originale: " ++ rleInput
  -- putStrLn $ "Données compressées: " ++ show rleCompressed
  -- case rleDecompressed of
  --   Just str -> putStrLn $ "Données décompressées: " ++ str
  --   Nothing -> putStrLn "Erreur lors de la décompression."

  -- -- Méthode Huffman
  -- putStrLn "Methods : Huffman\n"
  -- let input = "test bola CHIPOLATA 42! [ü]"
  --     (encodingTree, compressedBits) = compressHuffman input
  --     decompressedResult = decompressHuffman encodingTree compressedBits

  -- putStrLn $ "Original Input: " ++ show input
  -- putStrLn $ "Compressed Bits: " ++ show compressedBits
  -- putStrLn $ "Decompressed Result: " ++ show decompressedResult

  -- --Méthode ShannonFano
  -- putStrLn "\n\nMethode : ShannonFano\n"
  -- let str = "Est-ce que mon code marche ? Si affichage alors (System.out.printfln('yes') sinon throw(new callstackException()) :("
  -- let (tree,bit) = compressShannon str 
  -- let uncompressedStr = uncompressShannon tree bit

  -- putStrLn $ "Input : " ++ show str
  -- putStrLn $ "Compressed Input : " ++ show bit
  -- putStrLn $ "Uncompressed Input : " ++ show uncompressedStr

   -- test for the LZ78
  -- let inputString = "belle echelle !"
  --     compressed = LZ78.compress inputString
  --     decompressed = LZ78.uncompress compressed

  -- putStrLn "\n\nMethods : LZ78\n"
  -- putStrLn $ "Input String: " ++ inputString
  -- putStrLn $ "\nCompressed Codes: " ++ show compressed
  -- putStrLn "\nDecompressed Result:"
  -- case decompressed of
  --   Just result -> putStrLn result
  --   Nothing -> putStrLn "Unable to decompress the input"

  -- -- Méthode LZW
  -- putStrLn "\n\nMethods : LZW\n"
  -- let dataToCompress = "aaaabbcbbb"
  -- let compressedData = LZW.compress dataToCompress
  -- let decompressedData = LZW.uncompress compressedData

  -- putStrLn $ "Data originale: " ++ dataToCompress
  -- putStrLn $ "Données compressées: " ++ show compressedData
  -- case decompressedData of
  --   Just str -> putStrLn $ "Données décompressées: " ++ str
  --   Nothing -> putStrLn "Erreur lors de la décompression."
  
  -----------------------------------------------------------------------
  -----------------------------------------------------------------------
  -----------------------------------------------------------------------

    -- Demonstration Compression/Decompression Efficiencies for all methods
  putStrLn "\n\nDemonstration Compression/Decompression Efficiencies for all methods\n"

  putStrLn "Exemples of the PDF\n"
  putStrLn "Exemple 1 : \"aaaabbcbbb\" from slide 15 RLE"
  
  -- -- First Exemple with method RLE
  let originalInput = "aaaabbcbbb"
  afficher "RLE" originalInput
  afficher "Huffman" originalInput
  afficher "ShannonFano" originalInput
  afficher "LZ78" originalInput
  afficher "LZW" originalInput