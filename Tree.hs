data Tree a = EmptyTree | Node a (Tree a)(Tree a) deriving (Show)

singleton a :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree
