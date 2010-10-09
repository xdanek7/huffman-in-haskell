module Main (main)
where

import System.Environment (getArgs)
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import Data.Char
import Data.Word

import Huff 
import Cetnost
import Tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", soubor] ->  do text <- readFile soubor
                          encode (soubor ++ ".huf") text
    ["-d", soubor] ->  decode soubor
    _ -> putStrLn napoveda
      where
        napoveda = "\n***Huffman-in-Haskell***\nhttp://github.com/xdanek7/huffman-in-haskell\n\nPoužití:\n\t-c soubor.txt\t\t-- komprimuje soubor\n\t-d soubor.huf\t\t-- dekomprimuje soubor"


zakoduj soubor = do text <- readFile soubor
                    encode (soubor ++ ".huf") text



encode soubor text = do
  --stream <- IO.openBinaryFile soubor WriteMode
  handle <- IO.openBinaryFile soubor IO.WriteMode
  --let hlavicka = ByteString.pack [0x68, 0x75, 0x66, 0x66, 0x0D, 0x0A, 0x1A, 0x0A]
  let oddelovac = ByteString.pack [0x04]
  let strom = ByteStringChar8.pack $ show $ stromCetnosti text
  --let huff = ByteString.pack $ hEncode text
  --ByteString.hPut handle hlavicka
  --ByteString.hPut handle oddelovac
  ByteStringChar8.hPut handle strom
  ByteString.hPut handle oddelovac
  --ByteString.hPut handle huff
  IO.hClose handle
--  ByteString.hPut handle oddelovac
  
decode soubor = do
  --handle <- IO.openBinaryFile soubor IO.ReadMode
  bytestring <- ByteString.readFile soubor
  let oddelovac = ByteString.pack [0x04]
  let (strom':enc'') = ByteString.split 0x04 bytestring
  --putStrLn $ show $ length enc''
  let enc' = ByteString.intercalate oddelovac enc''
  --let enc' = enc''
 -- let spli' = ByteString.split 0x04 (spli !! 1)
  --putStrLn $ show strom
  let strom = read (map (chr . fromIntegral) $ ByteString.unpack strom') :: Strom Char
  --debugTree strom
  let enc = hDecode'' strom (ByteString.unpack enc')
  putStrLn enc
  --return ()
