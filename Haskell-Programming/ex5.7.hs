find:: Eq a => a -> [(a,b)]-> [b]
find k tbl = [ v | (k',v) <- tbl, k' == k ]


positions'::Eq a => a -> [a] -> [Int]
positions' num xs = find num tbl2 where tbl2 = [(n,i) | (n,i) <- zip xs [0..]]

-------------

factors :: Int -> [Int]
factors n = [x| x <- [1..n], n `mod` x == 0]

perfect ::Int -> Int
perfect n = sum [ x | x <- factors n , x /= n]

perfects:: Int -> [Int]
perfects n = [x | x <- [0..n], perfect x == x]

------

---7) list equivalence
rewrite = concat [ [(x,y) | y <- [3,4]] | x <- [1,2]]