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