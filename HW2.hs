import HW2types

-- Exercise 1 - Lists

-- a) Insert an element into a multiset
ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a, 1)]
ins a ((i, n):xs)
    | a == i = ((i,n+1):xs)
    | otherwise = (i, n): ins a xs

-- b) remove a single element from a multiset
del :: Eq a => a -> Bag a -> Bag a
del a [] = []
del a ((i, n):xs)
    | a == i && n > 1 = ((i,n-1):xs)
    | a == i = xs
    | otherwise = (i, n): del a xs

-- c) take a list of values and produce a multiset
bag :: Eq a => [a] -> Bag a 
bag [] = []
bag (x:xs) = ins x (bag xs)

-- d) determine if one subbag is contained in another
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag [] _ = True
subbag _ [] = False
subbag ((i, n):xs) b' 
    | n <= findTuple i b' = subbag xs b'
    | otherwise = False

-- Helper function - find matching tuple and return count
findTuple :: Eq a => a -> Bag a -> Int
findTuple a [] = 0
findTuple a ((i, n):xs)
    | a == i = n
    | otherwise = findTuple a xs

-- e) tests wether a bag is a set (only one of each)
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((i, n):xs) 
    | n == 1 = isSet xs
    | otherwise = False


-- f) compute the number of elements in a bag
size :: Bag a -> Int
size [] = 0
size ((i, n):xs) = n + size xs





-- Exercise 2 - Graphs

-- a) return the list of nodes contained in a graph
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm(fst x : snd x  : nodes xs)

-- b) compute list of sucessors for node in given graph
suc :: Node -> Graph -> [Node]
suc _ [] = []
suc node graph = [snd x | x <- graph, fst x == node]

-- c) detach a node from the graph
detach :: Node -> Graph -> Graph
detach _ [] = []
detach node graph = [x | x <- graph, fst x /= node, snd x /= node]

-- d) create a cycle of any given number
cyc :: Int -> Graph
cyc 0 = []
cyc len = [(x,((x `mod` len) + 1)) | x <- [1..len]]




-- Exercise 3 - Data Types

-- a) compute the width of a shape
width :: Shape -> Length
width (Pt a) = 0
width (Circle a b) = b * 2
width (Rect a b c) = max b c

-- b) compute the boudning box of a shape
bbox :: Shape -> BBox
bbox (Pt (x,y)) = ((x,y),(x,y))
bbox (Circle (x,y) r) = ((x-r, y-r), (x+r, y+r))
bbox (Rect (x,y) l w) = ((x,y),(x+l,y+w))

-- c) compute the minimum x coordinate of a shape
minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) r) = x-r
minX (Rect (x,y) l w) = x

-- d) move the position of a shape by a given vector 
move :: Shape -> Point -> Shape
move (Pt pt) vec = Pt(addPt pt vec)
move (Circle pt r) vec = (Circle (addPt pt vec) r) 
move (Rect pt l w) vec = (Rect (addPt pt vec) l w) 

-- helper function to add two points
addPt :: Point -> Point -> Point
addPt (x,y) (x',y') = (x+x',y+y')
