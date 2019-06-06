----ex6.8.hs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
					| otherwise = y: merge (x:xs) ys


--halve :: [a] -> ([a],[a])
--halve [] = [] 
--halve xs = splitAt n xs where n = length(xs) `div` 2

---msort :: Ord a => [a]->[a]
---msort [] = []
--msort [x] = [x]
---msort xs = merge( fst(halve xs) snd(halve xs))

----------------------------------------------------

leng :: [a] -> Int
leng [] = 0
leng (x:xs) = 1 + leng xs


drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 _ [] = []
drop1 n (x:xs) = drop1 (n-1) xs

init1 :: [a] -> [a]
init1 [] = []
init1 [x] = []
init1 (x:xs) = x:init1 xs

----------------------------------------------------

replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x:replicate1 (n-1) x

ind :: [a] -> Int -> a
ind xs 0 = head(xs)
ind (x:xs) n = ind xs (n-1)

elem1 :: Eq a=> a -> [a] -> Bool
elem1 _ [] = False
elem1 e (x:xs) | e==x = True
			   | otherwise = elem1 e xs

and1 :: [Bool] -> Bool
and1 [True] = True
and1 [False] = False
and1 (x:xs) | x == True = and1 xs
		   | otherwise = False

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

--------------------------------------------


				








