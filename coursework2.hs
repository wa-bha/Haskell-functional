-- Name: Bhavit Wadhwa
-- ID: 1516846

-- Q1: Halve function
-- Split the list at location: Length(list)/2
halve :: [a] -> ([a], [a])
halve list = splitAt ((length list) `div` 2) list

--------------------
--Q2: Meld function
meld :: [Int] -> [Int] -> [Int]
meld x [] = x
meld [] y = y
meld (x:xs) (y:ys) 
    | x <= y = x:meld xs (y:ys) 
    | x > y = y:meld (x:xs) ys

--------------------
--Q3: integerSort function
--Empty list and single element list are already sorted
integerSort :: [Int] -> [Int]
integerSort [] = []
integerSort [x] = [x]
integerSort list = meld 
    (integerSort(fst (halve list))) 
    (integerSort(snd (halve list)))

--------------------
--Q4: List comprehension (USE Main.replicate FOR TESTING)
--If the given length is 0, return the empty list
replicate :: Int -> a -> [a]
replicate 0 y = []
replicate x y = [y | i <- [1..x]]

--------------------
--Q5: The original definition [f x | x <- xs, p x]

-- MY COMPREHENSION:
-- For all values x in the list xs
-- Apply the filter/predicate constraint of function p x 
-- Apply all this to the list of values of function f to x

func :: (a -> a) -> [a] -> (a -> Bool) -> [a]
func f xs p = (map f . filter p) xs

--------------------
--Q6:
-- d2i [1,2,3] is processed by foldl as shown:
-- 10 * 0 + 1 = 1
-- 10 * 1 + 2 = 12
-- 10 * 12 + 3 = 123 + 0

d2i :: [Int] -> Int
d2i = foldl (\x y -> 10 * x + y) 0

--------------------
--Q7: Why do the following expressions not type-check?
--(i): [`a`,1,2]
-- Mix of types, should be a single type. eg: [1,2,3]
-- The a has backticks (`) rather than quotes to signify a string. 
-- [`a`] is not a valid type, backticks usually used for infix functions

--(ii): [(*), 0, (+)]
-- Lazy expression should be example: (*) 0 1 - each operator requires 2 curried arguments
-- Each operator requires seperate section (using commas) to evaluate together
-- eg: [(*) 1 2, (+) 1 2] evaluates to [2,3] - this would make it valid


--(iii): [(1,True),(`2`,False)]
-- Type is not valid for `2`, again, backticks usually used for infix functions however 2 is not a function


--(iv): length (1,2,3,4)
-- Use of parentheses instead of square brackets: length [1,2,3,4]
-- Length cannot fold on a tuple.
