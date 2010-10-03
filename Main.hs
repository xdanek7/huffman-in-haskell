module Main ()
where

import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    ["-c", soubor] ->  putStrLn ("-c " ++ soubor)
    ["-d", soubor] ->  putStrLn ("-d " ++ soubor)
    _ -> putStrLn napoveda
      where
        napoveda = "\n***Huffman-in-Haskell***\nhttp://github.com/xdanek7/huffman-in-haskell\n\nPoužití:\n\t-c soubor.txt\t\t-- komprimuje soubor\n\t-d soubor.huf\t\t-- dekomprimuje soubor"

