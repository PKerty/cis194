module Main where

{-
 -
 - fun1 :: [Integer] -> Integer
 - fun1 [] = 1
 - fun1 (x:xs)
 -      | even x = (x-2) * fun1 xs
 -      | otherwise = fun1 xs
 - -}

fun1 :: [Integer] -> Integer
-- la primer parte es filtrar la lista para que queden los pares
-- la segunda es restarle 2 a cada uno
-- la tercera es multiplicarlo entre ellos
fun1 = foldr (*) 1 . map (subtract 2) . filter even

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- un primer approach seria hacer un iterate de un valor y hacer takewhile de eso hasta que me devuelva 0
--

fun2 :: Integer -> Integer
fun2 =
    sum
        . filter even
        . takeWhile (> 1)
        . iterate (\x -> if even x then div x 2 else x * 3 + 1)

data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

--
-- foldTree :: [a] -> Tree a
-- foldTree xs = fst (build (length xs) xs)
--   where
--     ht Leaf           = -1
--     ht (Node h _ _ _) = h
--     mk l x r          = Node (1 + max (ht l) (ht r)) l x r
--
--     -- build n nodes from the front of the list, returning (tree, rest)
--     build 0 ys = (Leaf, ys)
--     build n ys =
--       let nl           = (n - 1) `div` 2
--           nr           = (n - 1) - nl
--           (l, y1)      = build nl ys
--           (x: y2)      = y1
--           (r, y3)      = build nr y2
--       in  (mk l x r, y3)
--
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs =
    let n = length xs
        nl = (n - 1) `div` 2
        (ls, rs) = splitAt nl xs
     in case rs of
            [] -> Leaf
            (x : rs') ->
                let l = foldTree ls
                    r = foldTree rs'
                    h t = case t of Leaf -> -1; Node k _ _ _ -> k
                 in Node (1 + max (h l) (h r)) l x r

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a l -> f a : l) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (\b g a -> g (f a b)) id xs z

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) filteredOdds
    where
        filteredOdds = filter (\x -> notElem x combinations) [1 .. n]
        combinations = [y + x + 2 * y * x | x <- [1 .. n], y <- [1 .. n]]
