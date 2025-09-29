module Main where

type Tower = String
type Move = (Tower, Tower)
            

main :: IO ()
-- | exerciseValue == 'b' = putStrLn (show (hanoi 4 "A" "C" "B"))
main = putStrLn (show (isValidCardNumber(sumDigits(splitValuesToDigits(doubleEverySecondValue(toDigitsRev  4012888888881881))))))


-- gets the digits from a number in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n == 0 = []
    | otherwise = mod n 10 : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n
    | n == 0 = []
    | otherwise = toDigits(div n 10) ++ [mod n 10]

doubleEverySecondValue :: [Integer] -> [Integer]
doubleEverySecondValue [] = []
doubleEverySecondValue (x:[]) = [x]
doubleEverySecondValue (x:y:t) = x : 2*y : doubleEverySecondValue(t)

splitValuesToDigits :: [Integer] -> [Integer]
splitValuesToDigits [] = []
splitValuesToDigits (x:t) = toDigits x ++ splitValuesToDigits t

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : t) = x + sumDigits t

isValidCardNumber :: Integer -> Bool
isValidCardNumber n = mod n 10 == 0

hanoi :: Integer -> Tower -> Tower -> Tower -> [Move]
hanoi n from to aux 
    | n == 0 = []
    | otherwise = (hanoi (n-1) from aux to) ++ [(from, to)] ++ (hanoi (n-1) aux to from)