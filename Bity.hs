module Bity
where

import Data.Bits
import Data.Word

-- typ pro uložení jednoho bitu
data Bit = H | L
  deriving (Eq, Read, Show)




rozdelBityPoOsmi :: [Bit] -> [[Bit]]
rozdelBityPoOsmi [] = []
rozdelBityPoOsmi x = takeBitsWithPadding 8 x : rozdelBityPoOsmi (drop 8 x)
  where
    takeBitsWithPadding :: Integer -> [Bit] -> [Bit]
    takeBitsWithPadding 0 _ = []
    takeBitsWithPadding n [] = L : takeBitsWithPadding (n-1) []
    takeBitsWithPadding n (x:xs) = x : takeBitsWithPadding (n-1) xs

prepracujBityNaBajty :: [[Bit]] -> [Data.Word.Word8]
prepracujBityNaBajty [] = []
prepracujBityNaBajty x = map spoj x
  where
    spoj :: [Bit] -> Data.Word.Word8
    spoj [] = 0      
    spoj (x:xs)
      | x == H = bit (length xs) .|. spoj xs
      | x == L = spoj xs

{--    
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H]
[240]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H]
[248]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H,H]
[252]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H,H,H]
[254]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H,H,H,H]
[255]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H,H,H,H,H]
[255,128]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H,H,H,H,H,H] 
[255,192]
(prepracujBityNaBajty . rozdelBityPoOsmi) [H,H,H,H,H,H,H,H,H,H,H]
[255,224]
(prepracujBityNaBajty . rozdelBityPoOsmi) [L]
[0]
(prepracujBityNaBajty . rozdelBityPoOsmi) [L,L]
[0]
(prepracujBityNaBajty . rozdelBityPoOsmi) [L,L,L,L,L,L,L,L]
[0]
(prepracujBityNaBajty . rozdelBityPoOsmi) [L,L,L,L,L,L,L,L,L]
[0,0]
(prepracujBityNaBajty . rozdelBityPoOsmi) [L,L,L,L,L,L,L,L,L,H] 
[0,64]
--}
