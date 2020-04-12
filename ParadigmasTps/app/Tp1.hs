-- comentario
import Data.List
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
--ver como tirar una excepcion aca si el primero numero es mas chico q el segundo
  | number1 < number2 = -1
  | otherwise = result
  where
    fn1 = factorial number1
    fn2 = factorial number2
    dif = number1 - number2
    fdif = factorial dif
    result = fn1 `div` (fn2 * fdif)
    
