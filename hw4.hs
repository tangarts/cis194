-- 
--
-- Homework 4
--


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)


-- foldTree :: [a] -> Tree a

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr step [] 
    where step x ys = f x : ys

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\ x -> 2*x+1) (filter (`notElem` p) [1..n])
        where p = [i+j+2*i*j | j <- [1..m], i <- [1..j]]
              m = n `div` 2 +1

fun1 :: [Int] -> Int
fun1 [] = 1
fun1 (x:xs)
        | even x    = (x - 2)*fun1 xs
        | otherwise = fun1 xs

fun1' :: [Int] -> Int
fun1' = product . map (\x -> x-2) . filter (even)


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3*n + 1)
