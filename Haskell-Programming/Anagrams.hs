-- Anagrams.               
-- 10th October, 2014
-- In Chapter 01
--_________________________________________________
-- To compile this program, type

-- $ ghc -o anagrams anagrams.lhs

-- This invokes the Glasgow Haskell Compiler (ghc)
-- and creates an executable file anagrams.o

-- You can then run the program with a command like

-- $ ./anagrams 6 < dictionary

-- This reads the file dictionary and prints the 6-
-- letter anagrams to the terminal. If you want the
-- result to go to an output file called "results",
-- you can type

-- $ ./anagrams 6 < dictionary > results

-- ________________________________________________
-- The functions toLower and sort are provided in
-- standard Haskell libraries:

import Data.Char -- (toLower)
import Data.List -- (sort,words)
import System.Environment -- (getArgs)
-- import Prelude hiding Word

-- We need getArgs because the ./anagrams command has
-- an argument, namely the number of letters in a word.

-- type Word  = [Char]
type Key   = [Char]

anagrams :: Int -> [String] -> String
anagrams n = showResult . groupByKey . sort .
              map addKey . getWords n

getWords :: Int -> [String] -> [String]
getWords n = filter ((==n) . length)

addKey :: String -> (Key,String)
addKey w = (sort w,w)

groupByKey :: [(Key,String)] -> [(Key,[String])]
groupByKey [] = []
groupByKey (kw:kws) = insert kw (groupByKey kws)
    where
      insert (k,w) []            = [(k,[w])]
      insert (k,w) ((k',ws):kws) = if k==k' then (k,w:ws):kws
                                   else (k,[w]):(k',ws):kws

showResult :: [(Key,[String])] -> String
showResult = concat . map showLine
    where
      showLine (k,ws) = k ++ ": " ++ addcommas ws ++ "\n"

addcommas :: [String] -> String
addcommas []     = []
addcommas [w]    = w
addcommas (w:ws) = w ++ "," ++ addcommas ws

main :: IO ()
main = do {[n] <- getArgs;
           text <- getContents;
           putStr (anagrams (read n) (words text)) }
