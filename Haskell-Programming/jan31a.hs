-- countdown k = [k, .., 1] for k >=1
-- otherwise   = []
-- use a local function helper
-- helper r returns [(n-r+1) , ... , 1] when r <= n
--                  []                  when r > n
   
countdown :: Int -> [Int]
countdown n       
    | n <= 0    =  []
    | otherwise =  helper 1 -- helper r returns [(n-r+1) , ... , 1] (*)
    where 
      helper          :: Int -> [Int]
      helper k 
          | k > n     = []
          | otherwise = (n-k+1)  : helper (k+1)      

-- reverse1 reverses a list without using the ++ function
-- use support functions
-- 1. mover

mover            :: ([a],[a]) -> ([a],[a])
mover (lst, [] )    =   (lst, [] )   -- base case   
mover (lst, (y:ys)) =  ((y:lst), ys) -- recursion step
 
-- 2. applyMover
-- apply the mover function k times 
-- 

applyMover :: Int -> ([a],[a]) -> ([a],[a])
applyMover k (x,y)
    | k <= 0    = (x,y)  
    | otherwise = mover ((applyMover (k-1) (x,y)))

-- 3. reverse1
-- obtain the reverse of a lst by 
-- moving the elements from the beginning of lst n times 
-- and extract the resulting lst 

reverse1 :: [a] -> [a]
reverse1 lst = fst (applyMover (length lst) ([], lst))

reverse2    :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse xs ++ [x]   -- reverse [1,2,3] = [3,2,1] = (reverse xs) ++ [1] (xs = [2,3]) 


f1 :: [(a,b)] -> [a]
f1 [] = []
f1 ((a,b):cs) = (a:(f1 cs))   -- f1 [(1,2), (3,4)] = 1:(f1 [(3,4)]) = 1: 3 : (f1 [])

myUnzip    :: [(a,b)] -> ([a],[b])
myUnzip []         =   ([],[])
myUnzip ((x,y):zs) =   ((x:fstzs),(y:sndzs))   -- ([2,3],[5,6]) ... (x:(fst ..) ,y:(snd ..))
    where 
      (fstzs, sndzs) = myUnzip zs