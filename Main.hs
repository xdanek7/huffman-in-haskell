module Main (main)
where

import System.Environment (getArgs)
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as ByteString



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", soubor] ->  encode soubor "pees"
    ["-d", soubor] ->  putStrLn ("-d " ++ soubor)
    _ -> putStrLn napoveda
      where
        napoveda = "\n***Huffman-in-Haskell***\nhttp://github.com/xdanek7/huffman-in-haskell\n\nPoužití:\n\t-c soubor.txt\t\t-- komprimuje soubor\n\t-d soubor.huf\t\t-- dekomprimuje soubor"

encode soubor text = do
  --stream <- IO.openBinaryFile soubor WriteMode
  ByteString.writeFile soubor bytestring
    where
      bytestring = ByteString.pack [0x00, 0x68, 0x75, 0x66, 0x66, 0x0D, 0x0A, 0x1A, 0x0A]
  
