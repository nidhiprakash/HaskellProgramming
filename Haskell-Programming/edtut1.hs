import Data.Char

-- • Problem 1.

-- Write a function halveEvens :: [Int] -> [Int], which takes as
-- argument a list of integers and returns half of each even integer
-- in that list.  For example halveEvens [0,2,1,7,8] == [0,1,4]

-- Your definition should use list comprehension, not recursion.
-- You may use the functions div, mod :: Int -> Int -> Int.

halveEvens ns = [ n `div` 2 | n <- ns, even n]


-- • Problem 2.

-- Again, write a function which takes as argument a list of
-- integers and returns half of each even integer in that list.  For
-- example halveEvensRec [0,2,1,7,8] == [0,1,4]

-- This time use recursion, not a list comprehension. You may use
-- div and mod again.

halveEvensRec [] = []
halveEvensRec (n:ns)
    | even n     = (n `div` 2):halveEvensRec ns
    | otherwise  = halveEvensRec ns

halvesEvensHT = (map (`div` 2)) . (filter even)                  

-- • Problem 3.

-- Write a function inRange :: Int -> Int -> [Int] -> [Int] to
-- return all numbers in the input list within the range given by
-- the first two arguments (inclusive). For example,

-- inRange 5 10 [1..15] == [5,6,7,8,9,10]

-- Your definition should use list comprehension, not recursion.

inRange low hi ns = [ n | n <- ns, low <= n, n <= hi]

-- • Problem 4.

-- Once again, write a function to return all numbers in the input
-- list within the range given by the first two arguments
-- (inclusive). For example,

-- inRangeRec 5 10 [1..15] == [5,6,7,8,9,10]

-- This time your definition should use recursion, not list comprehension.

inRangeRec _ _ []                              = []
inRangeRec low hi (n:ns) | low <= n && n <= hi = n : inRangeRec low hi ns
                         | otherwise           = inRangeRec low hi ns

inRangeHT low hi = filter (low <=) . filter (hi >=)


-- • Problem 5.

-- Write a function countPositives to count the positive numbers
-- (strictly greater than 0) in a list. For example, countPositives
-- [0,1,-3,-2,8,-1,6] == 3 Your solution should use a list
-- comprehension. You may not use recursion, but you will need a
-- specific library function.

countPositives ns = length [ n | n <- ns, n>0]

-- • Problem 6.

-- Again, write a function countPositivesRec to count the positive
-- numbers (strictly greater than 0) in a list. For example,
--   countPositivesRec [0,1,-3,-2,8,-1,6] == 3
-- Your function countPositivesRec should use recursion and no
-- library functions.  Why do you think it's not possible to write
-- countPositives using only list comprehension, without library
-- functions?

countPositivesRec []     = 0
countPositivesRec (n:ns) = (if n>0 then 1 else 0) + countPositivesRec ns

countPositivesHT = length . (filter (>0))                           


-- • Problem 7.

-- Professor Pennypincher will not buy anything if he has to pay
-- more than 199.00 Pounds. But, as a member of the Generous
-- Teachers Society, he gets a 10% discount on anything he
-- buys. Write a function
--   pennypincher :: [Int] -> Int
-- that takes a list of prices and returns the total amount that
-- Professor Pennypincher would have to pay, if he bought everything
-- that was cheap enough for him.

-- Prices should be represented in Pence, not Pounds, by
-- integers. To deduct 10% off them, you will need to convert them
-- into float first, using the function fromIntegral. To convert
-- back to Int, you can use the function round, which rounds to the
-- nearest integer. You can write a helper function
--   discount :: Int -> Int
-- to do this. Note that all your function definitions should
-- come with a type signature.

discount :: Int -> Int
discount p = round (0.9*fromIntegral p)
             
pennypincher :: [Int] -> Int
pennypincher ps = sum [ discount p | p <- ps, discount p <= 19900]
-- For example,
-- pennypincher [4500, 19900, 22000, 39900] == 41760

-- Your solution should use a list comprehension, and you may use a
-- library function to do the additions for you.

-- • Problem 8.

-- Again, Professor Pennypincher will not buy anything if he has to
-- pay more than 199.00 Pounds. But, as a member of the Generous
-- Teachers Society, he gets a 10% discount on anything he
-- buys. Write a function pennypincherRec :: [Int] -> Int that takes
-- a list of prices and returns the total amount that Professor
-- Pennypincher would have to pay, if he bought everything that was
-- cheap enough for him.

-- Prices should be represented in Pence, not Pounds, by
-- integers. To deduct 10% off them, you will need to convert them
-- into float first, using the function fromIntegral. To convert
-- back to Int, you can use the function round, which rounds to the
-- nearest integer. You can write a helper function discount :: Int
-- -> Int to do this. Note that all your function definitions should
-- come with a type signature.

-- For example,
-- pennypincherRec [4500, 19900, 22000, 39900] == 41760

-- This time your solution should use a recursion and no library functions.

pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (p:ps) | p'<= 19900 = p' + pennypincherRec ps
                       | otherwise  = pennypincherRec ps
                       where p' = discount p

pennypincherHT = sum . (filter (<= 19900)) . (map discount)                                  



-- • Problem 9.

-- Write a function multDigits :: String -> Int that returns the
-- product of all the digits in the input string. If there are no
-- digits, your function should return 1. For example,

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

-- Your definition should use list comprehension. You'll need a
-- library function to determine if a character is a digit, one to
-- convert a digit to an integer, and one to do the multiplication.

multDigits cs = product [ ord c - ord '0' | c <- cs, isDigit c]

-- • Problem 10.

-- Now write an equivalent function multDigitsRec that also returns
-- the product of all the digits in the input string. If there are
-- no digits, your function should return 1. For example,

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

-- This time use recursion. You may use library functions that act
-- on single characters or integers, but you may not use library
-- functions that act on a list.

multDigitsRec "" = 1
multDigitsRec (c:cs) | isDigit c = (ord c - ord '0') * multDigitsRec cs
                     | otherwise = multDigitsRec cs

multDigitsHT = product . map (+(-ord '0')) . map ord . filter isDigit

-- • Problem 11.

-- Write a function capitalise :: String -> String which, given a
-- word, capitalises it. That means that the first character should
-- be made uppercase and any other letters should be made
-- lowercase. For example,

-- capitalise "edINBurgH" == "Edinburgh"

-- Your definition should use a list comprehension and library
-- functions toUpper and toLower that change the case of a
-- character.

capitalise ""     = ""
capitalise (c:cs) = toUpper c : [toLower c' | c' <- cs]

-- • Problem 12.

-- Write another function, this time using recursion, capitaliseRec
-- which, given a word, capitalises it. That means that the first
-- character should be made uppercase and any other letters should
-- be made lowercase. For example,

-- capitaliseRec "edINBurgH" == "Edinburgh"

-- You may need to write a helper function; of the helper function
-- and the main function only one needs to be recursive. You must
-- write a type signature for each function you write.


capitaliseRec "" = ""
capitaliseRec (c:cs) = toUpper c : lower cs
    where lower ""     = ""
          lower (c:cs) = toLower c : lower cs

capitaliseHT  ""    = ""    
capitaliseHT (c:cs) = toUpper c : map toLower cs
                         
