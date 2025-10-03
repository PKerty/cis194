module Golf where

filterNth :: Int -> (a,Int) -> Bool
filterNth n (_,i) = mod i n == 0


someFilter :: ([(a,Int)], Int) -> [(a, Int)]
someFilter (xs, n) = filter (filterNth n) xs

skips :: [a] -> [[a]]
skips [] = []
-- al pedo escribir zip xs [1..length xs] porque haskell es lazy por default entonces no va a seguir creando elementos de la infinite series
-- Esta fue mi primer solucion, porque no conocia el famoso [ | <- , ]
-- ===========================================================================================
-- skips xs = map (map fst) (map someFilter (zip (replicate (length xs) (zip xs [1..])) [1..]))
-- ===========================================================================================
-- Conociendolo la idea es:
{-
 - primero: me quedo con los fst de una tupla (x,i) cuando i siendo entero te da modulo 0 con un determinado n
 - resul1(n) [ x | (x,i) <- myListOfTuples, mod i n]
 - segundo a estos les tengo que indicar el n entonces me quedo con los resultados de estos para cada vez que aplico un valor n de una lista de enteros crecientes (en este caso) aca no hay condicion
 - [resul1(n) | n <- [1..length xs]]
 - -}
skips xs = [[ x | (x,i) <- zip xs [1..], mod i n == 0] |  n <- [1..length xs]]

-- esto ultimo es como el de python, se choreo banda de cosas la serpiente trola
--
-- use recursiones soy un mono
localMaxima :: [Integer] -> [Integer]
-- localMaxima (x : y : z : xs)
    -- | x < y && y > z = y : localMaxima (z:xs)
    -- | otherwise = localMaxima (y:z:xs)
-- localMaxima _ = []
localMaxima xs = [ y | (x,y,z) <- zip3 xs (drop 1 xs) (drop 2 xs), x < y && y > z]

-- Not the prettiest
histogram :: [Integer] -> String
histogram [] = "\n==========\n0123456789\n"
histogram xs =   concat [[ if x >= m then '*' else ' ' |x <- amountPerValue] ++ "\n"| m <- reverse [1..maxOfValues]] ++ "==========\n" ++ "0123456789\n"
    where 
        amountPerValue = [ (sum l2) | l2 <- [[1 | x <- xs, x == n] | n <- [1..9]]]
        maxOfValues = maximum amountPerValue
-- chatgpt version
