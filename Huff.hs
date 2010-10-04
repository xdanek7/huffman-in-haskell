module Huff where

import Data.Word
import qualified Data.Map

import Tree
import Typy

-- zatím kašlu na bity a byty, budu je ukládat jako Chary a Stringy
prefixovaTabulka :: Tree Pismenko -> [(Char, String)]
--tohle je ok, v kořenu nebude nikdy uloženo pismeno, protože budeme ještě přidávat znak EOF
prefixovaTabulka t = prefixujNode "" t

prefixujNode :: String -> Tree Pismenko -> [(Char, String)]
prefixujNode prefix (Node a EmptyTree EmptyTree) = [(fst a, prefix)]
prefixujNode prefix (Node a l p) = [(fst a, prefix)] ++ leveprefixy ++ praveprefixy
	where
		leveprefixy = prefixujNode (prefix ++ "1") l 
		praveprefixy = prefixujNode (prefix ++ "0") p

-- Ukázka
--  stromCetnosti "pees"
--  prefixovaTabulka $ stromCetnosti "pees"
--   > [(' ',""),('e',"1"),(' ',"0"),('s',"01"),('p',"00")]

-- Ukázka s mapou (let mapa = ...)
--  Data.Map.fromList $ prefixovaTabulka $ stromCetnosti "pees"
--   > fromList [(' ',"0"),('e',"1"),('p',"00"),('s',"01")]
--  Data.Map.lookup 's' mapa
--     > Just "01"

--FIXME znak ' ' a EOF, jak je popsáno ve fixme v cetnost.hs



{-
   ** "Definujte funkce hEncode::String→[Word8] a hDecode::[Word8]→String, **
   *    které budou převádět znakové řetězce do binárních dat a naopak."    *
   **************************************************************************
-}

hEncode::String -> [Word8]
hEncode = undefined

hDecode::[Word8] -> String
hDecode = undefined
