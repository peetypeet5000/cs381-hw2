import HW2types

-- Question 1

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
