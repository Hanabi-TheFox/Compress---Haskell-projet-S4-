# compress
> Projet scolaire de Compression sans Perte

Ce projet est le fruit d'un effort collaboratif de Oussama El-Fehri, Marwane Lagzaoui, Nicolas Mendy, Ethan Pinto et Anthony Weiss dans le cadre d'un projet scolaire de Programmation Fonctionnelle pour CY Tech. L'objectif du projet est de mettre en œuvre divers algorithmes de compression sans perte, dont le codage RLE (Run-length encoding), Huffman, Shannon/Fano, le LZ78 et le codage de LZW.

## Table des matières

- [Aperçu](#aperçu)
- [Algorithmes Implémentés](#algorithmes-implémentés)
  - [RLE (Run-length Encoding)](#rle-codage-par-longueurs-successives)
  - [Codage de Huffman](#codage-de-huffman)
  - [Codage Shannon/Fano](#codage-shannonfano)
  - [LZ78](#lz78)
  - [LZW](#lzw)
- [Construction et Exécution](#construction-et-exécution)
- [Contributeurs](#contributeurs)
- [Licence](#licence)

## Aperçu

La compression sans perte est une technique utilisée pour réduire la taille des données sans perdre d'informations. Le projet se concentre sur la mise en œuvre de plusieurs algorithmes bien connus de compression sans perte.

## Algorithmes Implémentés

### RLE (Run-length Encoding)

[Le RLE](src/RLE.hs) est une forme simple de compression de données où les séquences de la même valeur de données sont stockées en tant que seule valeur de données avec leur compte.

### Codage de Huffman

[Le codage de Huffman](src/Statistic/Huffman.hs) est un algorithme de codage de longueur variable qui est utilisé pour la compression de données sans perte. Il fonctionne en attribuant des codes de longueur variable aux caractères d'entrée, avec des codes plus courts attribués aux caractères plus fréquents.

### Codage Shannon/Fano

[Le codage Shannon/Fano](src/Statistic/ShannonFano.hs) est un autre algorithme de codage de longueur variable similaire au codage de Huffman. Il fonctionne en divisant de manière récursive l'ensemble de symboles en deux sous-ensembles jusqu'à ce que chaque sous-ensemble ne contienne qu'un seul symbole.

### LZ78

[LZ78](src/LZ/LZ78.hs) est un algorithme de compression basé sur un dictionnaire qui remplace les occurrences répétées de données par des références à une seule copie de ces données existant plus tôt dans le flux de données non compressées.

### LZW

[LZW](src/LZ/LZW.hs) est un algorithme de compression basé sur un dictionnaire qui construit un dictionnaire de séquences fréquemment rencontrées et les remplace par des codes uniques.

## Construction et Exécution

Pour construire et exécuter le projet, suivez ces étapes :

1. Clonez le dépôt :

    ```bash
    https://github.com/Hanabi-TheFox/Compress---Haskell-projet-S4-.git
    ```

2. Accédez au répertoire du projet :

    ```bash
    cd compress
    ```

3. Construisez le projet en utilisant Cabal :

    ```bash
    cabal build
    ```

4. Exécutez la routine exécutable définie dans compress.cabal :

    ```bash
    cabal run compress-exe
    ```

## Contributeurs

- Oussama El-Fehri
- Marwane Lagzaoui
- Nicolas Mendy ([LinkedIn](https://www.linkedin.com/in/nicolas--dubois/) - [Mail](mendynicol@cy-tech.fr))
- Ethan Pinto ([LinkedIn](https://www.linkedin.com/in/ethan-pinto-/) - [Mail](pintoethan@cy-tech.fr))
- Anthony Weiss

## Licence

Ce projet est la propriété de CY Tech et Romain DUJOL - consultez le fichier [LICENSE](LICENSE) pour plus de détails.
