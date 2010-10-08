module Huff where

import Data.Word
import qualified Data.Map

import Tree
import Typy

-- typ pro uložení jednoho bitu
data Bit = H | L
  deriving (Eq, Read, Show)

prefixovaTabulka :: Strom a -> [(a, [Bit])]
--tohle je ok, v kořenu nebude nikdy uloženo pismeno, protože ještě přidáváme znak EOF
prefixovaTabulka t = prefixujNode [] t
  where
    prefixujNode :: [Bit] -> Strom a -> [(a, [Bit])]
    prefixujNode prefix (List v h) = [(h, prefix)]
    prefixujNode prefix (Uzel v l p) = leveprefixy ++ praveprefixy
      where
        leveprefixy = prefixujNode (prefix ++ [H]) l 
        praveprefixy = prefixujNode (prefix ++ [L]) p


{-
   ** "Definujte funkce hEncode::String→[Word8] a hDecode::[Word8]→String, **
   *    které budou převádět znakové řetězce do binárních dat a naopak."    *
   **************************************************************************
-}

hEncode::String -> [Word8]
hEncode = undefined

hDecode::[Word8] -> String
hDecode = undefined
