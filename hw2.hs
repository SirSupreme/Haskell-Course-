{-
Assignment 2
Junior Kabela
CIS 623
02/16/2026

-}

-- Question 1
-- Define the following functions by using recursion:
-- a) Multiplication of two integers in terms of addition
multiply :: Int -> Int -> Int
multiply x 0 = 0
multiply x y = x + multiply x (y - 1)

-- b) Division of two integers in terms of subtraction
-- A simple integer division (quotient) for non-negative divisor.
-- For divisor 0 we raise an error. This implementation assumes
-- non-negative dividend and divisor; extending to negatives is left
-- as an exercise.
divide :: Int -> Int -> Int
divide _ 0 = error "divide: division by zero"
divide x y
  | x < y     = 0
  | otherwise = 1 + divide (x - y) y

-- Question 2
-- Define a function to find the smallest element of a list of integers.
smallest :: [Int] -> Int
smallest [] = error "smallest: empty list"
smallest [x] = x
smallest (x:xs) = min x (smallest xs)

-- Question 3
{-Define a function that takes in a list of integers, adds all the members, divides the
sum by 10 and returns the remainder-}
remainder :: [Int] -> Int
remainder xs = (sum xs) `mod` 10

-- Question 4
{- A local maximum of a list is an element of the list which is strictly greater (i.e. not
smaller or equal) than both the elements immediately before and after it. For example,
in the list [2,3,4,1,5], the only local maximum is 4, since it is greater than the elements
immediately before and after it (3 and 1). The number 5 is not a local maximum since
there is no element that comes after it -}
maximum :: [Int] -> Int
maximum [] = error "maximum: empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)


-- end of file

