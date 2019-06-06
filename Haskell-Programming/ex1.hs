insert :: Int -> [Int] -> [Int]
insert a (x:xs) 
			| a<x = a:x:xs
			|otherwise = x:insert a xs

rotate' :: Int -> [Int] -> [Int]
rotate' a xs = ( drop b xs) ++ (take b xs)
					where b = mod a (length xs)
----------------------------------------
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
mirror :: (Ord a) => a -> Tree a -> Tree a  
mirror x EmptyTree = singleton x  
mirror x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a left (mirror x right)   
    | x > a  = Node a (mirror x left) right  

