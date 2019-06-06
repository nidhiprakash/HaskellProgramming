halve::[a]->([a],[a])
halve xs = splitAt mid xs 
			where mid = length(xs) `div` 2

---------------------------------------

null1::[a] ->Bool
null1 [] = True
null1 _ = False

safetail::[a]->[a]
safetail xs = if null1(tail xs)
				then [] else tail xs

---------------------------------------
luhnDouble :: Int -> Int
luhnDouble n = if 



	if gtr9 then (2*n) - 9 else (2*n) where gtr9 = if (2*n)>9 then True else False

checkifdiv:: Int -> Bool
checkifdiv num = if (num `mod` 10 == 0 )then True else False

luhn::Int->Int->Int->Int->Bool
luhn x y w z = if checkifdiv(check1) then True else False
				where 
					check1 = sum(map (luhnDouble) [x,y,w,z]) 


--------------

