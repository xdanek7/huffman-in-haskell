--cetnost :: [char] -> [(char, int)]

import Data.List
import Data.Function

spocitej_cetnost :: [Char] -> [(Char, Integer)]
spocitej_cetnost = (cetnost . sort)

cetnost :: [Char] -> [(Char, Integer)]
cetnost [a] = [(a, 1)]
cetnost (a:as) = (cetnost as)++[(a, 1)]

splacnout :: [(Char, Integer)] -> [(Char, Integer)]
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

setridit_splacnute :: [(Char, Integer)] -> [(Char, Integer)]
setridit_splacnute = sortBy (compare `on` snd)
