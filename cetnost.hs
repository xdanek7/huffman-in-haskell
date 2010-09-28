--cetnost :: [char] -> [(char, int)]

-- systémové
import Data.List
import Data.Function

-- moje
import Tree
import Typy

spocitej_cetnost :: Slovo -> [Pismenko]
spocitej_cetnost = (cetnost . sort)

cetnost :: [Char] -> [Pismenko]
cetnost [a] = [(a, 1)]
cetnost (a:as) = (cetnost as)++[(a, 1)]

splacnout :: [Pismenko] -> [Pismenko]
-- jak se líp ošetří pole délky 1?
splacnout (a)
	| length a == 1 = a
splacnout (a:b:bs) = if c1 == c2 then
	-- dokud je a i b stejné písmeno, dáváme je dohromady
		splacnout ([(c1, i1 + i2)] ++ bs)
	else
	-- necháme a za náma a posuneme se na b a zbytek
		[a] ++ splacnout ([b] ++ bs)
	where (c1, i1) = a
	      (c2, i2) = b

setridit_splacnute :: [Pismenko] -> [Pismenko]
setridit_splacnute = sortBy (compare `on` snd)

-- zatim umime udelat tohle
-- (setridit_splacnute . splacnout . spocitej_cetnost) "matematika"
-- > [('k',1),('i',1),('e',1),('t',2),('m',2),('a',3)]


