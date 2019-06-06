import Text.ParserCombinators.ReadP
import Data.List
import Data.Char

parse = readP_to_S    

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 6

data Stm = Action Act | | IfT Tst Stm | IfTE Tst Stm Stm
           deriving (Show)
data Act = A | B | C                      deriving (Show)
data Tst = P | Q | R                      deriving (Show)

stm = action +++ ifthen +++ ifthenelse
      
action = (symbol "a" >> return (Action A))
         +++ (symbol "b" >> return (Action B))
         +++ (symbol "c" >> return (Action C))             

test   = (symbol "p" >> return P)
         +++ (symbol "q" >> return Q)
         +++ (symbol "r" >> return R)
ifthen = do { symbol "if"
            ; tst <- test
            ; symbol "then"
            ; thenpart <- stm
            ; return $ Cond $ IfT tst thenpart
            }

ifthenelse = do { symbol "if"
                ; tst <- test
                ; symbol "then"
                ; thenpart <- stm
                ; symbol "else"
                ; elsepart <- stm
                ; return $ Cond $ IfTE tst thenpart elsepart
                }              
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 7
ps = (pa >> px) +++ (py >> pc)
pa = munch (=='a') >> accept
pc = munch (=='c') >> accept
px = (char 'b' >> px >> char 'c' >> accept ) <++ accept
py = (char 'a' >> py >> char 'b' >> accept ) <++ accept
accept = return ()

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 8

data Expr = Atom String | SExp [Expr] deriving (Eq)
          
instance Show Expr where
    show (Atom s)  = s
    show (SExp es) = "("++(intercalate " " (map show es))++")"

token p    = do { x <- p; skipSpaces; return x }
symbol str = token (string str)
ident      = do { c <- satisfy isLetter
                ; cs <- munch isAlphaNum
                ; return (c:cs)
                }

expr, atom, sexpr :: ReadP Expr

expr  = atom +++ sexpr

atom   = do { at <- token ident
            ; return $ Atom at
            }

sexpr = do { symbol "("
           ; es <- many expr
           ; symbol ")"
           ; skipSpaces
           ; return $ SExp es
           }

