module Cetnost
where

-- systémové
import Data.List
import Data.Function
import Data.Char

-- moje
import Tree
import Typy
--import Huff

spocitej_cetnost :: [Char] -> [Pismenko]
spocitej_cetnost = (splacnout. cetnost . sort)
  where
    cetnost :: [Char] -> [Pismenko]
    cetnost [] = error "Nic k zakódování"
    cetnost [a] = [(a, 1)]
    cetnost (a:as) = (cetnost as)++[(a, 1)]

    splacnout :: [Pismenko] -> [Pismenko]
    splacnout [a] = [a]
    splacnout (a:b:bs)
      | c1 == c2 = splacnout ([(c1, i1 + i2)] ++ bs) -- dokud je a i b stejné písmeno, dáváme je dohromady
      | otherwise = [a] ++ splacnout ([b] ++ bs) -- necháme a za náma a posuneme se na b a zbytek	
      where
        (c1, i1) = a
        (c2, i2) = b

-- A to vše směřuje k téhle funkci. Tadá!
--FIXME: potřebuji mít někde nějaký znak EOF, konce souboru, aby se dostal do kódovací tabulky
stromCetnosti :: [Char] -> Strom Char
stromCetnosti a = head $ vybuildiSuperStrom fronta
	where
		fronta = [vytvorList y x | (x,y) <- pole]
			where
				pole = (eot, 1) : spocitej_cetnost a
				-- znak End Of Transmission, ten použijeme jako znak konce vstupu.
				eot = chr 0x4


