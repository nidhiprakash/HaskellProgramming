-- CIS 623, Exercises 1

-- Problem 1 ----------------------------------------------------------

-- degenerate a b c 
--   tests whether the line ax+by+c=0 is degenerate
-- EXAMPLES
--   (degenerate 0 0 1) should return True
--   (degenerate 1 1 0) should return False
degenerate :: Float -> Float -> Float -> Bool
degenerate a b c = (a==0)&&(b==0)

-- TESTS
t2a = degenerate 0 0 1 -- should be True
t2b = degenerate 1 1 0 -- should be False
--   ****ADD MORE****

-- Problem 2 ----------------------------------------------------------

-- onLine (x1,y1) a b c 
--   tests whether (x1,y1) is on the line ax+by+c=0.
-- EXAMPLES
--   (onLine (1,1) 1 1 0) should return True
--   (onLine (1,1) 1 1 1) should return False
onLine :: (Float,Float) -> Float -> Float -> Float -> Bool
onLine (x,y) a b c = (a*x+b*y+c==0)

-- TESTS
t1a = onLine (1,1) 1 1 0  -- should be False
t1b = onLine (1,1) 1 1 1  -- should be False
t1c = onLine (2,5) (-2) 1 (-1) -- should be True  
--   ****ADD MORE****


-- Problem 3 -----------------------------------------------------------

-- horizonal a b c 
--   tests whether the line ax+by+c=0 is horizonal.
--   We assume the line is nondegenerate.
-- EXAMPLES
--   (horizonal 0 (-3) 4) should be True
--   (horizonal 1 1 12)   should be False
horizonal ::  Float -> Float -> Float -> Bool
horizonal a b c = (a==0)

-- TESTS
t3a = horizonal 0 (-3) 4 -- should be True
t3b = horizonal 1 1 12   -- should be False
--   ****ADD MORE****

-- Problem 4 -----------------------------------------------------------

-- vertical a b c 
--   tests whether the line ax+by+c=0 is vertical.
--   We assume the line is nondegenerate.

-- EXAMPLES
--   (vertical 3 0 (-2)) should return True
--   (vertical 1 1 12)   should return False
--   ****ADD MORE****
vertical  ::  Float -> Float -> Float -> Bool
vertical a b c = (b==0)

-- TESTS
t4a = vertical 3 0 (-2) -- should be True
t4b = vertical 1 1 12   -- should be False
--   ****ADD MORE****


-- Problem 5 -----------------------------------------------------------

-- xIntercept a b c 
--   = the x-intercept of the line ax+by+c=0, if it has one.
--   If the line is degenerate or horizonal, then 0.0 is returned.
-- EXAMPLES
--   (xIntercept 0 0 0) should return 0.0
--   ****ADD MORE****
xIntercept :: Float -> Float -> Float -> Float
xIntercept a b c = if a==0 then 0 else -c/a

-- TESTS
t5a = xIntercept 1 1 0 -- should be 0.0
--   ****ADD MORE****

-- Problem 6 -----------------------------------------------------------

-- yIntercept a b c 
--   = the y-intercept of the line ax+by+c=0, if it has one.
--   If the line is degenerate or vertical, then 0.0 is returned.
-- EXAMPLES
--   (yIntercept 0 0 0) should return 0.0
--   ****ADD MORE****
yIntercept :: Float -> Float -> Float -> Float
yIntercept a b c = if a==0 then 0 else -c/b

-- TESTS
t6a = yIntercept 1 1 0  --  should be 0.0
--   ****ADD MORE****

-- Problem 7 -----------------------------------------------------------

-- parallel a1 b1 c1 a2 b2 c2 
--   tests whether the two lines a1*x+b1*y+c1=0 and a2*x+b2*y+c2=0
--   are parallel.  We assume neither line is degenerate.
-- EXAMPLES
--  (parallel 1 2 24 3 6 (-9)) should return True
--   ****ADD MORE****

-- DEFINITION
parallel :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
parallel a1 b1 c1 a2 b2 c2 =  a1*b2 == a2*b1

-- TESTS
t7a = parallel 1 2 24 3 6 (-9) -- should be True
--   ****ADD MORE****


-- Problem 8 -----------------------------------------------------------

-- intersect a1 b1 c1 a2 b2 c2 
--  tests whether the two lines a1*x+b1*y+c1=0 and a2*x+b2*y+c2=0 intersect 
--  in a single point.  If either of the two lines are degenerate, 
--  then False is returned.
-- EXAMPLES
--   (intersect 1 1 0 (-1) 1 0) should be True
--   ****ADD MORE****
intersect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
intersect a1 b1 c1 a2 b2 c2 = not (parallel a1 b1 c1 a2 b2 c2)

-- TESTS
t8a = intersect 1 1 0 (-1) 1 0 --  should be True
--   ****ADD MORE****


-- Problem 9 -----------------------------------------------------------

-- intersectionPt a1 b1 c1 a2 b2 c2 
--   determines the point of intersection the two lines
--   a1*x+b1*y+c1=0 and a2*x+b2*y+c2=0, if they have one.
--   If either line is degenerate or if the lines do not 
--   intersect in a point, (0.0,0.0) is returned.
-- EXAMPLES
--   (intersectionPt 1 0 (-1) 0 1 (-1)) should return (1.0,1.0)
--   ****ADD MORE****
intersectionPt :: Float -> Float -> Float -> Float -> Float 
                  -> Float -> (Float,Float)  
intersectionPt a1 b1 c1 a2 b2 c2 =
    if intersect a1 b1 c1 a2 b2 c2
      then ((b1*c2-b2*c1)/det,(a2*c1-a1*c2)/det)
      else (0,0)
    where det = a1*b2-a2*b1
    
-- TESTS
-- The following is a helper function to do the tests here.  Given
-- a1, b1, c1, a2, b2, and c2, it finds the supposed intersection
-- point via intersectionPt and then checks that the result really
-- is on the two lines.  The ``where'' business in simply one way of
-- introducing local variables in Haskell.  We will cover it shortly.
checkIntersect a1 b1 c1 a2 b2 c2 
    = (a1*b2/=a2*b1) && (onLine (x,y) a1 b1 c2) && (onLine (x,y) a2 b2 c2)
      where (x,y) = intersectionPt a1 b1 c1 a2 b2 c2 

t9a = checkIntersect 1 0 (-1) 0 1 (-1) -- should be True
--   ****ADD MORE****


-- Problem 10 ----------------------------------------------------------

-- lineEqual a1 b1 c1 a2 b2 c2 
--   tests whether the two lines a1*x+b1*y+c1=0 and  a2*x+b2*y+c2=0 are 
--   nondegenerate and equal.
-- EXAMPLES
--   (lineEqual 1 1 1 2 2 2) should be True
--   ****ADD MORE****
lineEqual :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
lineEqual a1 b1 c1 a2 b2 c2
     = (parallel a1 b1 c1 a2 b2 c2) &&
       (xIntercept a1 b1 c1)==(xIntercept a2 b2 c2) &&
       (yIntercept a1 b1 c1)==(yIntercept a2 b2 c2) &&

-- TESTS
t10a = lineEqual 1 1 1 2 2 2 -- should be True
--   ****ADD MORE****

------------------------------------------------------------------------
------------------------------------------------------------------------

