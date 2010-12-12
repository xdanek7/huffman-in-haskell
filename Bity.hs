module Bity (Bit(..)
            ,bajtNaBity
            ,rozdelBityPoOsmi
            ,prepracujBityNaBajty
            ,prepracujBajtyNaBity
            )
where

import Data.Bits
import Data.Word

-- | Typ pro uložení jednoho bitu
-- | Pro práci s daty jako se sekvencí bitů jsem shledal datový typ Word8 nevhodný
data Bit = H                    -- ^ Představuje binarní jedničku : '1'
         | L                    -- ^ Představuje binarní nulu     : '0'
  deriving (Eq, Read, Show)

{-|
  Funkce 'rozdelBityPoOsmi' rozdělí seznam hodnot typu Bit na seznam seznamů Bitů, každý o stejné délce 8, tedy počet bitů v jednom bajtu.
  Jedná se o předpřipravenou funkci 'rozděl_bity'
-}
rozdelBityPoOsmi = rozdel_bity 8

{-|
  Funkce 'rozdel_bity' rozdělí seznam hodnot typu Bit na seznam seznamů Bitů, každý o stejné délce 'n'.
  Pokud je seznam 'x' prázdný je výstupem vždy prázdný seznam.
  Pokud délka seznamu není násobkem n, bude poslední n-tice zarovnána příslušným počtem hodnot L
-}
rozdel_bity :: Int         -- ^ n: Délka každé n-tice
            -> [Bit]       -- ^ x: Vstupní seznam
            -> [[Bit]]     -- ^ Výstup: prázdný seznam, nebo seznam seznamů Bitů, každý o stejné délce 'n'
rozdel_bity _ [] = []
rozdel_bity n x = takeBitsWithPadding n x : (rozdel_bity n (drop n x))
  where
    takeBitsWithPadding :: Int -> [Bit] -> [Bit]
    takeBitsWithPadding 0 _ = []
    takeBitsWithPadding n [] = L : takeBitsWithPadding (n-1) []
    takeBitsWithPadding n (x:xs) = x : takeBitsWithPadding (n-1) xs
{-|
  Seznam seznamů prvků typu Bit přepracuje na seznam prvků typu Word8.
  To poslouží pro zápis do souboru
-}
prepracujBityNaBajty :: [[Bit]]              -- ^ 'x': Seznam seznamů Bitů
                     -> [Data.Word.Word8]    -- ^ Výstup: Seznam bajtů v datovém typu Word8
prepracujBityNaBajty [] = []
prepracujBityNaBajty x = map spoj x
  where
    spoj :: [Bit] -> Data.Word.Word8
    spoj [] = 0      
    spoj (x:xs)
      | x == H = bit (length xs) .|. spoj xs
      | x == L = spoj xs

{-|
  Inverzní funkce k 'prepracuj_bity_na_bajty'
-}
prepracujBajtyNaBity :: [Data.Word.Word8]  -- ^ 
                     -> [[Bit]]            -- ^
prepracujBajtyNaBity = map bajtNaBity


--new_Bajt
--type bajt = [Bit] -- ^ ! právě 8prvkový seznam

{-|
  Převádí Word8 na seznam bitů (typ Bit)
-}
bajtNaBity :: Data.Word.Word8    -- ^ 'i': Bajt v typu Word8
           -> [Bit]              -- ^ Výstup: Bajt jako seznam Bitů
bajtNaBity i = map (f i) bb
  where
    bb = [7,6..0] 
    f :: Data.Word.Word8 -> Int -> Bit
    f i b = if testBit i b
            then H
            else L


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
