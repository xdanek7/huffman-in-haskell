module Huff where

import Data.Word
import Data.Maybe
import qualified Data.Map
import Data.List

import Cetnost
import Tree
import Typy
import Bity

prefixovaTabulka :: Strom a -> [(a, [Bit])]
--tohle je ok, v kořenu nebude nikdy uloženo pismeno, protože prázdný řetězec odmítneme a jinak ještě přidáváme znak EOT
prefixovaTabulka t = prefixujNode [] t
  where
    prefixujNode :: [Bit] -> Strom a -> [(a, [Bit])]
    prefixujNode prefix (List v h) = [(h, prefix)]
    prefixujNode prefix (Uzel v l p) = leveprefixy ++ praveprefixy
      where
        leveprefixy = prefixujNode (prefix ++ [H]) l 
        praveprefixy = prefixujNode (prefix ++ [L]) p

-- Odmítám se babrat se stromem prefixů. A hašová tabulka je v idealním případě dokonce lepší - O(1)
-- Jenže na dekodování to asi potřebovat budu
prefixovaMapa = (Data.Map.fromList . prefixovaTabulka)


{-
   ** "Definujte funkce hEncode::String→[Word8] a hDecode::[Word8]→String, **
   *    které budou převádět znakové řetězce do binárních dat a naopak."    *
   **************************************************************************
-}

hEncode::String -> [Word8]
hEncode text = prepracujBityNaBajty $ rozdelBityPoOsmi seznambitu
  where
    strom = stromCetnosti text
    mapa = prefixovaMapa strom
    seznambitu = foldr krok [] text'
      where krok x acc = (fromJust (Data.Map.lookup x mapa)) ++ acc
            text' = text ++ ['\EOT']

hEncode''':: Strom Char -> String -> [Word8]
hEncode''' strom text = prepracujBityNaBajty $ rozdelBityPoOsmi seznambitu
  where
    mapa = prefixovaMapa strom
    seznambitu = foldr krok [] text'
      where krok x acc = (fromJust (Data.Map.lookup x mapa)) ++ acc
            text' = text ++ ['\EOT']

hDecode::[Word8] -> String
hDecode = undefined

--dekoduje první písmenko
hDecode'' :: Strom Char -> [Word8] -> [Char]
hDecode'' strom bajty = dekodujStromem strom seznambitu
  where
    seznambitu = concat $ prepracujBajtyNaBity bajty
    dekodujStromem :: Strom Char -> [Bit] -> [Char]
    dekodujStromem (List v h) bity
      | h /= '\EOT' = h : dekodujStromem strom bity
      | otherwise = []
    dekodujStromem strom bity
      | head bity == H = dekodujStromem (_levy strom) (tail bity)
      | head bity == L = dekodujStromem (_pravy strom) (tail bity)
      
{--

let text = "semprase"
let strom = stromCetnosti text
let enc = hEncode text

hDecode'' strom enc
> "semprase" !!!!!!!!!!!!!!!!!!!!!!!!!! JO!
-}
