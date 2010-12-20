module Tree (Strom(..)
            ,vytvorList
            ,spojStromy
            ,zatridStromDoSeznamu
            ,vybuildiSuperStrom
            ,debugTree
            ,zacinaUzlem
            )
where

import Typy
import Data.List

{-|
  Binární strom
-}
data Strom a = List {vaha :: Integer,
                     hodnota :: a}
             | Uzel {vaha :: Integer,
                     _levy :: (Strom a),
                     _pravy :: (Strom a)
                     }
             deriving (Eq, Ord, Show, Read)
{-|
  Vytvoří nový prvek typu Strom pomocí konstruktoru List
  FIXME: Dost zbytečné, stačí volat normálně List s úplně stejnými parametry
-}
vytvorList :: Integer             -- ^ 'váha'': váha listu
           -> a                   -- ^ 'hodnota'': hodnota v něm uložená
           -> Strom a             -- ^ Výsledek: Nový list
vytvorList vaha' hodnota' = List {vaha = vaha'
                                  ,hodnota = hodnota'}

spojStromy :: Strom a -> Strom a -> Strom a
spojStromy a b = Uzel {vaha = vaha a + vaha b
                        ,_levy = a
                        ,_pravy = b}

{-|
  Předpokládá, že seznam už je setříděný vzestupně
  FIXME: Nepodařilo se mi napsat to bez požadavku na porovnatelnost a,
    přitom by na ní vůbec nemělo záležet
  A navíc to ani není nikde v použito, protože v knihovně už je funkce 'insert'
  
  Ukázka:
    let prvek = List 6 'e'
    let pole = [List 5 'f', List 20 'g']
    zatridStromDoSeznamu prvek pole
     > [List {vaha = 5, hodnota = 'f'},
     >  List {vaha = 6, hodnota = 'e'},
     >  List {vaha = 20, hodnota = 'g'}]
-}
zatridStromDoSeznamu :: (Ord a) => Strom a -> [Strom a] -> [Strom a]
zatridStromDoSeznamu a xs = mensi ++ [a] ++ vetsi
	where
		mensi = [c | c <- xs, c < a || c == a]
		vetsi = [d | d <- xs, d > a]

{-|
  Vytvoří prefixový strom tak, že rekurzivně v seznamu spojuje stromy s nejmenší váhou
-}
vybuildiSuperStrom :: (Ord a) => [Strom a]   -- ^ Setříděný seznam Listů
                              -> [Strom a]   -- ^ Prefixový strom
vybuildiSuperStrom (a:[]) = [a]
vybuildiSuperStrom (a:b:[]) = [spojStromy a b]
vybuildiSuperStrom (a:b:cs) = vybuildiSuperStrom $ insert (spojStromy a b) cs

zacinaUzlem :: Strom a -> Strom a
zacinaUzlem t@(List _ _) = spojStromy t t
zacinaUzlem t@(Uzel _ _ _) = t

{-|
  Vypise strom cetnosti alespon trochu slusnym zpusobem
-}
debugTree :: (Show a) => Strom a        -- ^ Strom
                      -> IO ()          -- ^ IO akt vypsání storomu
debugTree = (putStr . unlines . vypisUzel)
  where
    vypisUzel :: (Show a) => Strom a -> [String]
    vypisUzel (List v h) = ["| " ++ show h ++ " : " ++ show v]--["Váha: " ++ show v ++ " hodnota: " ++ show h]
    vypisUzel (Uzel v l p) = ["`\\"] ++ levy ++ pravy
	where
		levy = map ("\t" ++) (vypisUzel l) 
		pravy = map ("\t" ++) (vypisUzel p)

