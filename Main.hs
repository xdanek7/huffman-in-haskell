module Main (main)
where

import System.Environment (getArgs)
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import Data.Char
import Data.Word

import Huff
import Tree
import Typy

{-|
  Hlavní funkce programu huffman-in-haskell.
  Program slouží ke kompresi souborů pomocí Huffmanova kódování.
  Funkce/program přečte předaný soubor a podle toho, zda dostala parametr -c nebo -d na jeho obsah zavolá buďto funkci encode, nebo decode.
  
  parametr -c ... komprimuje soubor
  parametr -d ... dekomprimuje soubor
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", soubor] ->  do text <- readFile soubor
                          encode (soubor ++ ".huf") text
    ["-d", soubor] ->  decode soubor
    _otherwise -> putStrLn napoveda
      where
        napoveda = "\n***Huffman-in-Haskell***\nhttp://github.com/xdanek7/huffman-in-haskell\n\nPoužití:\
         \\n\t-c soubor.txt\t\t-- komprimuje soubor\n\t-d soubor.huf\t\t-- dekomprimuje soubor"

{-|
  Zakóduje 'text' huffmanovým kódem a uloží výsledek do souboru 'soubor'.
-}
encode soubor text = do
  -- otevře soubor
  handle <- IO.openBinaryFile soubor IO.WriteMode
  -- připraví si jednotlivé části výstupního souboru:
  let oddelovac = ByteString.pack [mAGIE]
  let strom = stromCetnosti text
  let strom_b = ByteStringChar8.pack $ show strom
  let zakodovany_b = ByteString.pack $ hEncode''' strom text
  -- soubor se skládá z textové reprezentace stromu (datové struktury Strom v Tree.hs) (v ASCII)
  ByteStringChar8.hPut handle strom_b
  -- následně oddělovače (mAGIE v Typy.hs)
  ByteString.hPut handle oddelovac
  -- a nakonec binárních dat
  ByteString.hPut handle zakodovany_b
  IO.hClose handle

{-|
  Vrátí text po dekódování souboru 'soubor'
-}
decode soubor = do
  bytestring <- ByteString.readFile soubor
  let oddelovac = ByteString.pack [mAGIE]
  let (strom':enc'') = ByteString.split mAGIE bytestring
  -- v programu nijak systemově neřešíme situaci, kdy se bajt mAGIE objeví ve výstupu Huffmanova kódování.
  -- V reprezentaci stromu se objevit nemůže (mAGIE je schválně zvolen jako netisknutelný znak ASCII)
  -- když se objeví v datech, vede to k tomu, že funkce split rozdělí na více než dvě části.
  -- proto druhou část a dále zase slepíme, čímž to napravíme
  let enc' = ByteString.intercalate oddelovac enc''
  let strom = read (map (chr . fromIntegral) $ ByteString.unpack strom') :: Strom Char
  let enc = hDecode'' strom (ByteString.unpack enc')
  putStr enc
--  putStrLn ""

