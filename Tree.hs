module Tree where

import Typy

data Tree a = EmptyTree | Node {hodnota :: a,
		levy :: (Tree a),
		pravy :: (Tree a)
		} deriving (Show) --,Ord

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

buildTree :: Pismenko -> Tree Pismenko
buildTree a = singleton a

spojStromy :: Tree Pismenko -> Tree Pismenko -> Tree Pismenko
spojStromy a b = Node (' ', vaha) a b
	where
		vaha = snd (hodnota a) + snd (hodnota b)
--Ukázka:
--let a = singleton ('r', 6)
--let b = singleton ('v', 5)
--spojStromy a b
-- >Node {hodnota = (' ',11), levy = Node {hodnota = ('r',6), levy...

-- Předpokládá, že fronta už je setříděná vzestupně
zatridStromDoFronty :: Tree Pismenko -> [Tree Pismenko] -> [Tree Pismenko]
zatridStromDoFronty a xs = mensi ++ [a] ++ vetsi
	where
		-- FIXME: Vyřešit tohle porovnávání Písmenek (,)
		mensi = [c | c <- xs, snd (hodnota c) < snd (hodnota a) || snd (hodnota c) == snd (hodnota a)]
		vetsi = [d | d <- xs, snd (hodnota d) > snd (hodnota a) ]

-- Ukázka:
--  let prvek = singleton('g', 6)
--  let pole = [singleton ('e', 5), singleton('f', 7)]
--  zatridStromDoFronty prvek pole
--   > [Node {hodnota = ('e',5), levy = EmptyTree, pravy = EmptyTree},
--   >  Node {hodnota = ('g',6), levy = EmptyTree, pravy = EmptyTree},
--   >  Node {hodnota = ('f',7), levy = EmptyTree, pravy = EmptyTree}]
