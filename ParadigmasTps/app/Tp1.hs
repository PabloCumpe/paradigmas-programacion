-- comentario
import Prelude hiding (max, min)
import System.IO
-- ghci
-- :l archivo
-- :r -> compilar
-- :t funcion -> ver definicion de funcion
-------------------------------------------
signo :: Int -> Int
signo number
  | number > 0 = 1
  | number == 0 = 0
  | otherwise = -1

--------------------------------------------
negativo :: Int -> Bool
negativo number = signo number == -1

--------------------------------------------
max :: Int -> Int -> Int
max number1 number2
  | number1 > number2 = number1
  | otherwise = number2

--------------------------------------------
max3 :: Int -> Int -> Int -> Int
max3 number1 number2 number3
  | (number1 > number2) && (number1 > number3) = number1
  | (number2 > number1) && (number2 > number3) = number2
  | otherwise = number3

---------------------------------------------
min :: Int -> Int -> Int
min number1 number2
  | max number1 number2 == number1 = number2
  | otherwise = number1

-------------------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial number = number * factorial (number - 1)

-------------------------------------------------
-- C (n,m) = m! / n! (m-n)!
combinatorio :: Integer -> Integer -> Integer
combinatorio number1 number2
--ver como tirar una excepcion aca si el primer numero es mas chico q el segundo
  | number1 < number2 = -1
  | otherwise = result
  where
    fn1 = factorial number1
    fn2 = factorial number2
    dif = number1 - number2
    fdif = factorial dif
    result = fn1 `div` (fn2 * fdif)

-----------------------------------------------
divisiblePor :: Integer -> Integer -> Bool
divisiblePor number1 number2 = number1 `mod` number2 == 0
-----------------------------------------------
newFold :: (a -> b -> b) -> b -> [a] -> b
newFold  f acc [] = acc
newFold f acc (x:xs) = f x (newFold f acc xs)
esVacia :: [x] -> Bool
esVacia list = newFold(\x n -> 1 + n) 0 list == 0

---------------------------------------------------------
cabeza :: [a] -> a
cabeza (x:xs) = x
---------------------------------------------------------
resto :: [a] -> [a]
resto (x:xs) = xs
----------------------------------------------------------
long :: [a] -> Integer
long list = newFold(\x n -> 1 + n) 0 list

----------------------------------------------------------
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-------------------------------------------------------------

member :: Integer -> [Integer] -> Bool
member number [] = False
member number (x:xs)
   | number == x = True
   | otherwise = member number xs


--------------------------------------------------------------------------
append :: [a] -> [a] -> [a]
append x [] = x
append [] x = x 
append (x:xs) (a:as) = x : a : append xs as

----------------------------------------------------------------------------

tomar :: Integer -> [a] -> [a]
tomar 0 _  = []
tomar number list = 
    newFold function newList list 0
    where
    function x r i | i >= number      = []
                   | otherwise = x : r (i+1)
    newList _ = []