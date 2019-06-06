-- Activity 2 for CIS 623
-- Trees (Test 2 related)
-- 4/15/2019

-- Given the BTree datatype declared as follows 
-- (or import from BinaryTrees.hs)

-- a datatype for binary trees
data BTree a = Empty 
             | BNode a (BTree a) (BTree a)
               deriving (Show)


-- a datatype for directions
data Dir = Lft | Rght
type Path = [Dir]


-- Write the following functions:

-- 1. height 

{--

A. Write a Haskell function

height :: BTree a -> Int 

such that height tree calculates the height of tree. 
For an empty tree, return -1.

--}


{--

B. Write a Haskell function

leaves :: BTree a -> [a]

such that leaves tree returns a list containing the 
labels of all of tree's leaf nodes.

--}


{--

C. Write a Haskell function

sameShape :: BTree a -> BTree b -> Bool

such that sameShape t1 t2 determines whether the 
trees t1 and t2 have identical structures 
(i.e., they have exactly the same shape, but they may
have different labels at the corresponding nodes). 
For example, tree2 and tree4 from the BinaryTrees module 
have the same shape.


--
-- tree2:         8
--               / \
--              13  20
--                \
--                 12
--


--
-- tree4:      "here's"
--               / \
--             "a" "string"
--                \
--                 "tree"
--


--}


{--

D. Write a Haskell function

follow :: Path -> BTree a -> BTree a

such that (follow path tree) returns the subtree 
that you arrive at by following the path in tree. 
Note that (follow path Empty) should always
return Empty.

--}




























{-- 
Suggested answers
--}


-- A

height :: BTree a -> Int
height Empty                    = -1
height (BNode label left right) = 1 + 
                                  max (height left) (height right)

-- B

leaves :: BTree a -> [a]
leaves Empty = []
leaves (BNode val Empty Empty) = [val]
leaves (BNode _ left right) = leaves left ++ leaves right


-- C

sameShape :: BTree a -> BTree b -> Bool
sameShape Empty Empty = True
sameShape (BNode v1 left1 right1) (BNode v2 left2 right2) 
              =  sameShape left1 left2 && sameShape right1 right2
sameShape _ _ =  False


-- D

follow :: Path -> BTree a -> BTree a
follow [] tree = tree
follow path Empty = Empty
follow (Lft:path) (BNode _ left _) = follow path left
follow (Rght:path) (BNode _ _ right) = follow path right
