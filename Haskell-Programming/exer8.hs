data Tree a = Empty | Fork (Tree a) a (Tree a) Int  deriving(Show)

addTree :: Ord a => (Tree a) -> a -> (Tree a)
addTree Empty a = Fork Empty a Empty 0
addTree (Fork lt b rt n) c
				| (c<b) = Fork (addTree lt c) b rt (n+1)
				| otherwise = Fork lt b (addTree rt c) n



index :: Ord a => (Tree a)-> a -> Maybe Int
index Empty a = Nothing
index (Fork lt b rt n ) c 
				| (c == b) = Just n
				| (c< b) = index lt c
				| otherwise = fmap (+(n+1)) (index rt c)

fetch :: Ord a => Tree a-> Int -> Maybe a
fetch Empty _ = Nothing
fetch (Fork lt b rt n) i
			| (i == n) = Just b
			| (i< n) = fetch lt i
			| otherwise = fetch rt (i-(n+1))

reroot :: Ord a => (Tree a) -> a -> Tree a
reroot Empty _ = Empty
reroot (Fork lt b rt n) c
				| (c<b) = rightrot (Fork (reroot lt c) b rt n)
				|otherwise = leftrot (Fork lt b (reroot rt c) n)

rightrot , leftrot :: (Tree a) -> (Tree a)
rightrot ( Fork (Fork a p b m) q c n) = Fork a p (Fork b q c (n-m-1)) m  
leftrot  (Fork a p (Fork b q c n) m ) = Fork (Fork a p b m) q c (m+n+1)
