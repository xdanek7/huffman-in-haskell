module Cetnost
where

-- systémové
import Data.List
import Data.Function
import Data.Char
import Data.Maybe

-- moje
import Tree
import Typy
--import Huff

{- Funkci mi poradili na #haskell @ freenode, moje původní byla strašně pomalá-}
spocitej_cetnost :: [Char] -> [Pismenko]
spocitej_cetnost = map (\xs -> (head xs, fromIntegral (length xs))) . group . sort

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



{-
Hello people. I am trying to count how many times each character occurs in a given string and all my attempts yet have been pathetically slow
	<jirka>	any suggestions how to do this efficiently? please?
	<Philippa>	use an appropriate data structure to store the character frequencies?
	<Cale>	> map (\xs -> (head xs, length xs)) . group . sort $ "mississippi"
	<lambdabot>	[('i',4),('m',1),('p',2),('s',4)]
	<jirka>	wow, you are awesome! 0.5M characters in less then a second!
	<EvanR>	ghc optimizes beautiful code more ;)
	<ddarius>	EvanR: That code is certainly not beautiful for what it does.
	<EvanR>	haha
	<Philippa>	it does something non-obvious, amongst other things
	<Cale>	> map (head &&& length) . group . sort $ "mississippi"
	<lambdabot>	[('i',4),('m',1),('p',2),('s',4)]
	<jirka>	apparently. I managed only 20K characters in about half a minute. For whole book it would take a whole life, I guess
	<Cale>	jirka: What algorithm were you using?
	* ddarius	is not even sure how jirka could do that.
	<silver>	sorting is godlike
	<Philippa>	ddarius: list abuse
	<jirka>	on first try I wrote a recursive function
	<Philippa>	take the obvious algorithm and use /really inappropriate/ data structures
	<ddarius>	Treat lists as arrays.
	<EvanR>	treat Arrays as arrays ;)
	<Cale>	as mutable arrays :)
	<jirka>	it took first two pairs from a sorted list, decided whether they are equal, If so, combined them into one and called itself over the result... something like that
	<jirka>	should I post it somewhere? Probably not...
	<Cale>	ah, okay
	<jirka>	Cale: BTW, what does &&& ?
	<Cale>	f &&& g = \x -> (f x, g x)
	<Cale>	It's from Control.Arrow, and actually is a bit more general than the code I just wrote.
	<jirka>	well, It looks almost like a magic to me
	<silver>	@hoogle group
	<lambdabot>	Data.ByteString group :: ByteString -> [ByteString]
	<lambdabot>	Data.List group :: Eq a => [a] -> [[a]]
	<lambdabot>	Data.ByteString.Char8 group :: ByteString -> [ByteString]
	<Cale>	> (head &&& length) "aaaaa"
	<lambdabot>	('a',5)
	<Palmik>	pretty nifty :)
	<Cale>	> group "iiiimppssss"
	<lambdabot>	["iiii","m","pp","ssss"]
	<EvanR>	haskell is 25% magic and 5% more magic
	<EvanR>	.. 75
	<arcasin>	@type ***
	<lambdabot>	parse error on input `***'
	<arcasin>	@type (***)
	<lambdabot>	forall (a :: * -> * -> *) b c b' c'. (Arrow a) => a b c -> a b' c' -> a (b, b') (c, c')
	<EvanR>	:t (***)
	<lambdabot>	forall (a :: * -> * -> *) b c b' c'. (Arrow a) => a b c -> a b' c' -> a (b, b') (c, c')
	<arcasin>	:t (&&&)
	<lambdabot>	forall (a :: * -> * -> *) b c c'. (Arrow a) => a b c -> a b c' -> a b (c, c')
	<arcasin>	oh i see
	* hackagebot	archlinux 0.3.3 - Support for working with Arch Linux packages http://hackage.haskell.org/package/archlinux-0.3.3 (DonaldStewart)
	* hackagebot	cabal2arch 0.7.4 - Create Arch Linux packages from Cabal packages http://hackage.haskell.org/package/cabal2arch-0.7.4 (DonaldStewart)
	<McManiaC>	> (head *** (+1)) ([1..5],2)
	<lambdabot>	(1,3)
	<McManiaC>	ok :)
	<ski>	@type snd &&& fst
	<lambdabot>	forall a c. (a, c) -> (c, a)
	<McManiaC>	hehe
	<arcasin>	:t arr
	<lambdabot>	forall b c (a :: * -> * -> *). (Arrow a) => (b -> c) -> a b c
	<EvanR>	gah
	<EvanR>	how do i compose C -> D with A -> B -> C to get A -> B -> D
	<Saizan>	?pl \f g x y -> f (g x y)
	<lambdabot>	(.) . (.)
	<Saizan>	?pl \x y -> f (g x y)
	<lambdabot>	(f .) . g
	* EvanR	types (f .) . g into the ghci
	<aristid>	EvanR: (.:) = (.).(.)
	<EvanR>	i feel like im programming in braile now
	* Twey	prefers ‘fmap f . g’
-}
