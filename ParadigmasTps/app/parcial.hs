-- comentario
{-# LANGUAGE BlockArguments #-}

import Prelude hiding (max, min)
import System.IO

-- ghci
-- :l archivo
-- :r -> compilar
-- :t funcion -> ver definicion de funcion
--
--
-- Definir una funciï¿½n pares, que dado un nï¿½mero entero positivo, devuelva una lista de pares de nï¿½meros enteros positivos, con el primer componente del par menor o igual que el segundo componente, cuya suma sea igual al nï¿½mero entero positivo dado. Por ejemplo:
--		pares 7 = [(1,6),(2,5),(3,4)]
--		pares 10 = [(1,9),(2,8),(3,7),(4,6),(5,5)]
pares :: (Ord a, Eq a, Num a) => a -> [(a, a)]
pares num = obtenerPares (1, num - 1) num

obtenerPares :: (Ord a, Eq a, Num a) => (a, a) -> a -> [(a, a)]
obtenerPares (a, b) c
  | a > b = []
  | a + b == c = (a, b) : obtenerPares (a + 1, b - 1) c
  | otherwise = obtenerPares (a + 1, b - 1) c