countDown :: Int -> [Int]
countDown 0 = []
countDown n = n:countDown (n-1)

countUp :: Int ->[Int]
countUp k

	| k<=0 = []
	| k > 0 = helper k

helper:: Int -> [Int]
helper k 
	|k>0 = reverse(k:helper(k-1))
	| otherwise = []