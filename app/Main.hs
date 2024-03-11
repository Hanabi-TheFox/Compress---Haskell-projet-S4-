module Main (main) where

import RLE -- Create by Oussama Marwane Hamza
import Statistic.Huffman as Huffman -- Create by Nicolas Mendy
import Statistic.ShannonFano as ShannonFano -- Create by Marwane Laghzaoui
import LZ.LZ78 as LZ78 -- Create by Ethan Pinto
import LZ.LZW as LZW -- Create by Weiss Anthony

-- Fonction pour afficher le texte compressé et décompressé selon la méthode passé en paramètre.
afficher :: String -> String -> IO ()
afficher methode input = do
  putStrLn $ "\n---------------------\nMéthode : " ++ methode ++ "\n"
  -- putStrLn $ "Input :\n\n" ++ input ++ "\n"
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

    _ -> putStrLn $ "Méthode non reconnue : " ++ methode ++ "\n"
  where
    afficherResultats :: Show a => a -> Maybe String -> IO ()
    afficherResultats compressed decompressed = do
      putStrLn $ "Données compressées:\n\n" ++ show compressed ++ "\n"
      case decompressed of
        Just str -> do
          -- putStrLn $ "Données décompressées:\n\n" ++ str ++ "\n"
          let success = str == input
          putStrLn $ "Décompression sans perte: " ++ show success ++ "\n"
        Nothing -> putStrLn "Erreur lors de la décompression.\n"
      

main :: IO ()
main = do

    -- Demonstration Compression/Decompression Efficiencies for all methods
  putStrLn "\n\nDemonstration Compression/Decompression Efficiencies for all methods\n"

  putStrLn "Exemples of the PDF\n"
 
  -- First Exemple with method RLE
  putStrLn "=====================\nExemple 1 : \"aaaabbcbbb\" from slide 15 RLE" 
  afficher "RLE" "aaaabbcbbb"
  afficher "Huffman" "aaaabbcbbb"
  afficher "ShannonFano" "aaaabbcbbb"
  afficher "LZ78" "aaaabbcbbb"
  afficher "LZW" "aaaabbcbbb"
  -- Second Exemple with method Huffman & ShannonFano
  putStrLn "=====================\nExemple 2 : \"abbca\" from slide 26 Huffman"
  afficher "RLE" "abbca"
  afficher "Huffman" "abbca"
  afficher "ShannonFano" "abbca"
  afficher "LZ78" "abbca"
  afficher "LZW" "abbca"
  -- Third Exemple with method LZ78 & LZW
  putStrLn "=====================\nExemple 3 : \"belle echelle !\" from slide 31 LZ78"
  afficher "RLE" "belle echelle !"
  afficher "Huffman" "belle echelle !"
  afficher "ShannonFano" "belle echelle !"
  afficher "LZ78" "belle echelle !"
  afficher "LZW" "belle echelle !"

  putStrLn "\nOther exemples\n"
  -- Fourth Exemple with a random binary string
  let exemple4 = "1101010101101001001010101110110100101011010010111101001010110011101100101111001010101010110111101101011011100100101101010111110101101101110011010111011010101011001101011010100"
  putStrLn $ "=====================\nExemple 4 : random binary string : \"" ++ exemple4 ++ "\""
  afficher "RLE" exemple4
  afficher "Huffman" exemple4
  afficher "ShannonFano" exemple4
  afficher "LZ78" exemple4
  afficher "LZW" exemple4

  -- Fifth Exemple with the first paragraph of "Haskell" from wikipedia
  let exemple5 = "Haskell est un langage de programmation fonctionnel fondé sur le lambda-calcul et la logique combinatoire. Son nom vient du mathématicien et logicien Haskell Curry. Il a été créé en 1990 par un comité de chercheurs en théorie des langages intéressés par les langages fonctionnels et l'évaluation paresseuse. Le dernier standard est Haskell 2010 : c'est une version minimale et portable du langage conçue à des fins pédagogiques et pratiques, dans un souci d'interopérabilité entre les implémentations du langage et comme base de futures extensions. Le langage continue d'évoluer en 2020, principalement avec GHC, constituant ainsi un standard de facto comprenant de nombreuses extensions."
  putStrLn $ "=====================\nExemple 5 : \"" ++ exemple5 ++ "\""
  afficher "RLE" exemple5
  afficher "Huffman" exemple5
  afficher "ShannonFano" exemple5
  afficher "LZ78" exemple5
  afficher "LZW" exemple5