import Data.Char

-----------pairs using list comprehensions---------

pairs:: [Int] ->[Int] ->[(Int,Int)]
pairs xs ys = [(x,y)| x<-xs,y<-ys]

------------- Concatenation of list of lists ---------

concat1 :: [[a]] -> [a]
concat1 xss = [x| xs<-xss,x<-xs]


-------------------Intro to wildcard in list comprehensions------
--Checking length of an array ---

leng :: [Int]->Int
leng xs = sum[1|_<-xs]

------ elimination of numbers --------
--getting firsts in pairs------

firsts :: [(a,b)] -> [a]
firsts xs = [x|(x,_)<-xs]

----------------------- Factors and prime numbers --------------

---1) if even------

allevens :: Int -> [Int]
allevens n = [x|x<-[0..n],even x]

---2) all Factors--------

factors :: Int -> [Int]
factors n = [x| x <- [1..n], n `mod` x == 0]

---3)  if prime or not ---

prime :: Int -> Bool
prime n = if factors n == [1,n] then True else False

---4) all primes till n

allprimes :: Int -> [Int]
allprimes n = [x| x <- [2..n], prime x]



---------- Implementing key-value pair data structure ---------------

find :: Eq a => a -> [(a,b)] -> [b]
find k tbl = [v | (k',v) <- tbl, k' == k]



------------- Zip functionalities with list comprehensions --------------

--1) pair all adj elemets

pairadj :: Ord a => [a] -> [(a,a)]
pairadj xs = [(x,y)| (x,y) <- zip xs (tail xs)]

--2) check if sorted

sorted :: Ord a => [a] -> Bool
sorted xs = and[x<=y | (x,y) <- pairadj xs]

--3) obtain positions 

position :: Ord a => a -> [a] -> [Int]
position x xs = [i | (x',i) <- zip xs [0..], x' == x]

---------------- Strings -----------------------------------------

--1) to convert to lower case


--lowercase :: String -> [Char]
--lowercase xs = [x | x <- xs , toLower x]

--2) counts the lowercased letters

clowers:: String -> Int 
clowers xs = length[x|x<-xs, isLower x]

countofchar :: Char -> String -> Int
countofchar x xs = length[ x' | x' <- xs, x' == x]

------------------- Cipher method ----------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let num = chr(num + ord 'a')

shiftchar:: Char -> Int -> Char
shiftchar c n | isLower c = int2let((let2int c + n) `mod` 26)
			  | otherwise = c

----encode
shiftString :: String -> Int -> String
shiftString xs n = [shiftchar x n | x <- xs]

rotate:: [a] -> Int -> [a]
rotate xs n = drop n xs ++ take n xs





