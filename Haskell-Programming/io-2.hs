----------------------------------------------------------------------
--
--  A way to read integers (and also floats)
--
----------------------------------------------------------------------

getInteger :: IO Integer
getInteger = do line <- getLine
                return (read line :: Integer)

getFloat :: IO Float
getFloat = do line <- getLine
              return (read line :: Float)





sample :: IO String
sample = do putStrLn "Please enter a character: "
            ch <- getChar
            putStrLn "Thank you!"
            return (replicate 10 ch)


{-

main :: IO ()
main = do putStr "Enter a string: "
          resp <- getLine
          putStrLn ("Your string was: " ++ resp)
          putStrLn "\n\nGoodbye!"

-}


sumN :: IO Float
sumN = do putStrLn "How many numbers do you want to sum? "
          n <- getInteger
          vals <- sequence [ putStr "Next float:" >> getFloat | x <- [1..n] ]
          return (sum vals)
