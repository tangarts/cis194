-- 
-- Homework 3
--

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]


localMaxima :: [Int] -> [Int]
localMaxima (x:body@(y:z:_))
        | y > z && y > x = y : localMaxima body
        | otherwise = localMaxima body
localMaxima _ = []

histogram :: [Int] -> String

histogram _ = "\n==========\n0123456789\n"

