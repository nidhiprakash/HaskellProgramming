newtype Angle = Angle { getAngle :: Int } deriving (Show)

instance  Eq Angle  where
	Angle a1 == Angle a2 = (a1-a2) `mod` 360 == 0

instance Monoid Angle where
	mempty  = Angle 0
	(Angle a1) `mappend` (Angle a2) = Angle ((a1+a2) `mod` 360)

	---------------------------------------------------------

newtype Priority = Priority { getPriority :: Int } deriving (Show)

instance Eq Priority where
	(Priority a) == (Priority b) = (max 0 a) == (max 0 b)

instance Ord Priority where
	(Priority a) <= (Priority b) = (max 0 a) <= (max 0 b)

instance Monoid Priority where
	mempty = Priority 0
	Priority a `mappend` Priority b = Priority (max 0 (a `max` b))

	------------------------------------------------------------

add3 :: (Maybe Int)->(Maybe Int)->(Maybe Int)->(Maybe Int)
add3 ma mb mc = do 
	{
	a <- ma;
	b <- mb;
	c <- mc;
	return ( a+b+c )
	}

--------------------------------------------------------------------

fun1 :: String -> IO ()
fun1 xs = (foldr (>>) (return ()) (map putChar xs)) 

fun2 :: String -> IO ()
fun2 xs = (foldl (>>) (return ()) (map putChar xs)) >> putChar '\n'

------------------------------------------------------------------

import System.IO

main = do
	withFile "check1.txt" Readmode (\handle -> do
		contents <- hGetcontents handle;
		putStr contents )



 




	 


