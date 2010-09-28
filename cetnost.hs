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

-- buildění fronty
--  let pole = (setridit_splacnute . splacnout . spocitej_cetnost) "pees"
--  let fronta = [buildTree x | x <- pole]
--  zpracujFrontu fronta
--   > [Node {hodnota = (' ',4),
-->		levy = Node {hodnota = ('e',2),
-->			levy = EmptyTree,
-->			pravy = EmptyTree},
-->		pravy = Node {hodnota = (' ',2),
-->			levy = Node {hodnota = ('s',1),
-->				levy = EmptyTree,
-->				pravy = EmptyTree},
-->			pravy = Node {hodnota = ('p',1),
-->				levy = EmptyTree,
-->				pravy = EmptyTree}}}]


zpracujFrontu :: [Tree Pismenko] -> [Tree Pismenko]
zpracujFrontu (a:[]) = [a]
zpracujFrontu (a:b:[]) = [spojStromy a b]
zpracujFrontu (a:b:cs) = zpracujFrontu $ zatridStromDoFronty (spojStromy a b) cs

-- A to vše směřuje k téhle funkci. Tadá!
stromCetnosti :: [Char] -> [Tree Pismenko]
stromCetnosti a = zpracujFrontu fronta
	where
		fronta = [buildTree x | x <- pole]
			where
				pole = (setridit_splacnute . splacnout . spocitej_cetnost) a
