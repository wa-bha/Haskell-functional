-- Name: Bhavit Wadhwa
-- ID: 1516846

-- 1.
-- a) (t -> t) -> t -> t
-- twice is a function type

twice f x = f (f x)
square n = n * n
naturals = [0..]
ones = 1 : ones

-- b) False
-- c) square :: Num a => a -> a
-- For any numeric type a, 
-- function square takes a value of type a and 
-- returns a value of type a as well.

-- d) twice square :: Num t => t -> t

-- e) 625
-- f) head :: [a] -> a
-- g) head naturals :: Integer
-- h) 0

-- 2. (3 *) :: Num a => a -> a

-- 3. \x -> x * 3

-- 4. square (square 3)

-- 5. map (2*) naturals

-- 6.
capitalize :: Char -> Char
capitalize ch = toEnum(if fromEnum ch >= 97 && fromEnum ch <= 122 
    then (fromEnum ch - 32) 
    else fromEnum ch)

-- 7.
capitalize_string :: String -> String
capitalize_string str = map capitalize str

-- 8.
or_list :: [Bool] -> Bool
or_list [] = False
or_list (x:xs) = x || or_list xs

-- 9.i)
flock :: Integer -> String
flock k = concat["sheep\n" | x <- [1..k]]

-- ii)
flock2 :: Integer -> String
flock2 k = concat (replicate (fromIntegral k) "sheep\n")

-- iii)
a_row_of_sheep :: Integer -> String
a_row_of_sheep k = concat["sheep " | x <- [1..k]]

-- iv)
big_flock :: Integer -> String
-- big_flock k = concat[concat[concat["sheep " | z <- [1..(x)]] ++ "\n"] | x <- [1..k]]

-- More efficient to re-use the a_row_of_sheep
big_flock k = concat[a_row_of_sheep x ++ "\n" | x <- [1..k]]