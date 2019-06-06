import Data.Char

--
--  an example of the bind operator
--

doubleLine :: IO ()
doubleLine = getLine >>= (\w -> putStrLn (w++w))    

echo :: IO ()
echo = getChar >>= putChar

--
--  The chain operator:
--      act1 >> act2
--          perform action act1, and then perform action act2
--

--
--  putNTimes n cs
--     Display the string cs n times (one copy per line, with "*****" as
--     a separator between lines)
--
{-
putNTimes :: Int -> String -> IO ()
putNTimes n str 
   | n <= 1    = putStrLn str 
   | otherwise = putStrLn str >> putStrLn "*****" >> putNTimes (n-1) str
-}

--
--  An alternate (but equivalent!) version of putNTimes, using do notation
--
--     IMPORTANT NOTE: Make sure the actions within a do-block all
--          line up with one another, or the interpreter will complain.

putNTimes :: Int -> String -> IO ()
putNTimes n str 
   | n <= 1    = putStrLn str 
   | otherwise = do putStrLn str 
                    putStrLn "*****"
                    putNTimes (n-1) str



getAndPut :: IO ()
getAndPut = do line <- getLine
               putStrLn line


----------------------------------------------------------------------

--
--  A way to read integers (and also floats)
--

getInteger :: IO Integer
getInteger = do line <- getLine
                return (read line :: Integer)

getFloat :: IO Float
getFloat = do line <- getLine
              return (read line :: Float)


----------------------------------------------------------------------

--
-- Let's write a Haskell program that:
--
--    (i) Prompts user to enter three integers
--
--   (ii) Reads in the three integers 
--
--  (iii) Displays their sum (preceded by useful output msg)
--
--   (iv) Returns a tuple containing the three integers
--


----------------------------------------------------------------------
sumThree :: IO (Integer, Integer, Integer)
sumThree = do putStrLn "Please enter three integers: "
              x <- getInteger
              y <- getInteger
              z <- getInteger
              putStrLn ("Their sum is " ++ show (x + y + z))
              return (x,y,z)
             
--
-- Let's write a Haskell program that:
--
--    (i) Reads a series of integers
--
--   (ii) Stops accepting input when it sees an even number
--
--  (iii) Returns the number of odd numbers seen
--
--





countOdds :: IO Int
countOdds = do n <- getInteger
               if even n 
                  then return 0
                  else do count <- countOdds
                          return (1 + count)



--
--  Reads in a string, 
-- and determines whether or not it's a palindrome,
--  and then displays a message indicating which.
--

isPalindrome :: String -> Bool
isPalindrome str =  (result == reverse result)
   where
     result = map toLower (filter isAlpha str)




pals :: IO ()
pals = do putStrLn "Please enter a string: "
          line <- getLine
{-
          if isPalindrome line
             then putStrLn ("Yes, " ++ line ++ "is a palindrome")
             else putStrLn "Alas, it was not a palindrome"
-}
          putStrLn (palAnswer (isPalindrome line) line)

palAnswer :: Bool -> String -> String
palAnswer True cs = "Yes, " ++ cs ++ " is a palindrome"
palAnswer _ cs = "Alas, it was not a palindrom"
