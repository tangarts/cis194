--
--
-- Homework 1


toDigits :: Int -> [Int]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Int -> [Int]
toDigitsRev 0 = []
toDigitsRev x = reverse $ toDigits x

doubleEveryOther' :: [Int] -> [Int]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:xs) = x : 2*y : doubleEveryOther' xs 

doubleEveryOther = reverse . doubleEveryOther' . reverse

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = (sumDigits xs) + sum (toDigits x)

validate :: Int -> Bool
validate cardNumber 
        = (sumDigits $ doubleEveryOther $ toDigits cardNumber) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a


