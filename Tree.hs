module Tree where

import Typy
import Data.List

data Strom a = List {vaha :: Integer,
                     hodnota :: a}
             | Uzel {vaha :: Integer,
                     _levy :: (Strom a),
                     _pravy :: (Strom a)
                     } deriving (Eq, Ord, Show, Read)
                 
vytvorList :: Integer -> a -> Strom a
vytvorList vaha' hodnota' = List {vaha = vaha'
                                  ,hodnota = hodnota'}

spojStromy :: Strom a -> Strom a -> Strom a
spojStromy a b = Uzel {vaha = vaha a + vaha b
                        ,_levy = a
                        ,_pravy = b}

-- Předpokládá, že seznam už je setříděný vzestupně
-- FIXME: Nepodařilo se mi napsat to bez požadavku na porovnatelnost a,
   -- přitom by na ní vůbec nemělo záležet
zatridStromDoSeznamu :: (Ord a) => Strom a -> [Strom a] -> [Strom a]
zatridStromDoSeznamu a xs = mensi ++ [a] ++ vetsi
	where
		mensi = [c | c <- xs, c < a || c == a]
		vetsi = [d | d <- xs, d > a]

vybuildiSuperStrom :: (Ord a) => [Strom a] -> [Strom a]
vybuildiSuperStrom (a:[]) = [a]
vybuildiSuperStrom (a:b:[]) = [spojStromy a b]
vybuildiSuperStrom (a:b:cs) = vybuildiSuperStrom $ insert (spojStromy a b) cs
		
-- Ukázka:
--  let prvek = List 6 'e'
--  let pole = [List 5 'f', List 20 'g']
--  zatridStromDoSeznamu prvek pole
--   > [List {vaha = 5, hodnota = 'f'},
--   >  List {vaha = 6, hodnota = 'e'},
--   >  List {vaha = 20, hodnota = 'g'}]

-- Vypise strom cetnosti alespon trochu slusnym zpusobem
debugTree :: (Show a) => Strom a -> IO ()
debugTree = (putStr . unlines . vypisUzel)
  where
    vypisUzel :: (Show a) => Strom a -> [String]
    vypisUzel (List v h) = ["| " ++ show h ++ " : " ++ show v]--["Váha: " ++ show v ++ " hodnota: " ++ show h]
    vypisUzel (Uzel v l p) = ["`\\"] ++ levy ++ pravy
	where
		levy = map ("\t" ++) (vypisUzel l) 
		pravy = map ("\t" ++) (vypisUzel p)

