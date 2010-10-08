module Huff where

import Data.Word
import Data.Maybe
import qualified Data.Map

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
prefixovaMapa = (Data.Map.fromList . prefixovaTabulka)

{-
   ** "Definujte funkce hEncode::String→[Word8] a hDecode::[Word8]→String, **
   *    které budou převádět znakové řetězce do binárních dat a naopak."    *
   **************************************************************************
-}

--hEncode::String -> [Word8]
hEncode text = prepracujBityNaBajty $ rozdelBityPoOsmi seznambitu
  where
    mapa = prefixovaMapa $ stromCetnosti text
    seznambitu = foldl krok [] text
      where krok acc x = acc ++ (fromJust (Data.Map.lookup x mapa))

hDecode::[Word8] -> String
hDecode = undefined
