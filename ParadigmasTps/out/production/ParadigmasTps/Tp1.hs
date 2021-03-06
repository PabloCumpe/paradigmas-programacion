-- comentario
{-# LANGUAGE BlockArguments #-}

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
  | number1 < number2 = -1
  | otherwise = result
  where
    fn1 = factorial number1
    fn2 = factorial number2
    dif = number1 - number2
    fdif = factorial dif
    result = fn1 `div` (fn2 * fdif)

--ver como tirar una excepcion aca si el primer numero es mas chico q el segundo
-----------------------------------------------
divisiblePor :: Integer -> Integer -> Bool
divisiblePor number1 number2 = number1 `mod` number2 == 0

-----------------------------------------------
newFold :: (a -> b -> b) -> b -> [a] -> b
newFold f acc [] = acc
newFold f acc (x:xs) = f x (newFold f acc xs)

esVacia :: [x] -> Bool
esVacia [] = True
esVacia (_:_) = False

---------------------------------------------------------
cabeza :: [a] -> a
cabeza (x:xs) = x

---------------------------------------------------------
resto :: [a] -> [a]
resto (x:xs) = xs

----------------------------------------------------------
long :: [a] -> Integer
long list = newFold (\x n -> 1 + n) 0 list

----------------------------------------------------------
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-----------------------------------------------------------
member :: Integer -> [Integer] -> Bool
member number [] = False
member number (x:xs)
  | number == x = True
  | otherwise = member number xs

---------------------------------------------------------------------------
append :: [a] -> [a] -> [a]
append x [] = x
append [] x = x
append (x:xs) (a:as) = x : a : append xs as

----------------------------------------------------------------------------
tomar :: Integer -> [a] -> [a]
tomar 0 _ = []
tomar number list = newFold function newList list 0
  where
    function x list acc
      | acc >= number = []
      | otherwise = x : list (acc + 1)
    newList _ = []

----------------------------------------------------------------------------
term :: Int -> [a] -> a
term number list = f list 1
  where
    f :: [a] -> Int -> a
    f (x:xs) acc =
      if acc == number
        then x
        else f xs (acc + 1)

-----------------------------------------------------------------------------
rev :: [a] -> [a]
rev list = f list []
  where
    f :: [a] -> [a] -> [a]
    f [] l = l
    f (x:xs) l = f xs (x : l)

--------------------------------------------------------------------------------
maxl :: [Int] -> Int
maxl [x] = x
maxl (x:xs)
  | maxl xs > x = maxl xs
  | otherwise = x
---------------------------------------------------------------------------------------------
cuenta n [] = 0
cuenta n (x:xs) = (if n==x then 1 else 0) + cuenta n xs

-----------------------------------------------------------------------------------------------------
repite :: a -> Int -> [a]
repite a 0 = []
repite a c = a : repite a (c-1)

-----------------------------------------------------------------------------------------------------------
(←→) :: Integer -> Integer -> [Integer]
(←→) number1 number2 =
  if (number1 > number2)
    then []
    else (number1 : ((number1 + 1) ←→ number2))

--otra forma de hacerlo
(←--→) number1 number2 = f number1 []
    where
    f :: Integer -> [Integer]-> [Integer]
    f number1 list
            | number1 <= number2 = number1 : f (number1+1) list
            | otherwise = list


-----------------------------------------------------------------------------------------------------------------

--recursion explicita -> cuando se ve si , por ej f llama a f
--recursion implicita -> cuando no se ve, porque uso otra funcion q esa si tiene recursion

factorial2 :: Integer -> Integer
factorial2 n = f (1 ←→ n)
      where 
      f :: [Integer] -> Integer
      f [] = 1
      f (x:xs) = x * f xs
      
--otra forma de hacerlo
factorial3 n = product (1 ←→ n)
--------------------------------------------------------------------------------------------------------------------
--ej 15
addAll :: a -> [[a]] -> [[a]]
addAll x [] = []
addAll a (x:xs) =  (a:x) : (addAll a xs)

partes [] = [[]]
partes (x:xs) = (partes xs) ++ (addAll x (partes xs))


-----
--ej 16--  
--1- f es un int
--2- f es una terna
--3- f 