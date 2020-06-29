-- comentario
{-# LANGUAGE BlockArguments #-}

import Prelude
import System.IO

-- ghci
-- :l archivo
-- :r -> compilar
-- :t funcion -> ver definicion de funcion
--
--
-- Definir una funciï¿½n pares, que dado un nï¿½mero entero positivo, devuelva una lista de pares de nï¿½meros enteros positivos, con el primer componente del par menor o igual que el segundo componente, cuya suma sea igual al nï¿½mero entero positivo dado. Por ejemplo:
--		pares 7 = [(1,6),(2,5),(3,4)]
--		pares 10 = [(1,9),(2,8),(3,7),(4,6),(5,5)] usar monada lista

--a-
pares :: (Ord a, Eq a, Num a) => a -> [(a, a)]
pares 0 = []
pares 1 = []
pares num = obtenerPares (1, num - 1) num
obtenerPares :: (Ord a, Eq a, Num a) => (a, a) -> a -> [(a, a)]
obtenerPares (a, b) c
  | a > b = []
  | a + b == c = (a, b) : obtenerPares (a + 1, b - 1) c
  | otherwise = obtenerPares (a + 1, b - 1) c

--b
paresUsandoMonada num = enumFromTo 1 (div num 2) >>= (\x -> [(x,num-x)])

{-
3)	i)	Definir el tipo de datos ArbolNRot, que representa un ï¿½rbol n-ario donde los nodos poseen un valor de un tipo dado, y donde los arcos (rï¿½tulos) que unen un nodo con cada subï¿½rbol tambiï¿½n poseen un valor de eventualmente otro tipo dado. Considerar que el tipo puede ser paramï¿½trico.
	ii)	Definir la funciï¿½n de orden superior foldrANR que dado un ï¿½rbol ArbolNRot en forma anï¿½loga a la funciï¿½n foldr de las listas (recursiï¿½n de pila), y que posea parï¿½metros adecuados que apliquen a este tipo de datos.
	iii)	Definir la funciï¿½n rotulosRamas, que dado un ï¿½rbol ArbolNRot con nodos de un tipo arbitrario y rï¿½tulos enteros, retorne una lista de enteros donde cada elemento entero se asocia a cada rama del ï¿½rbol y corresponde a la suma de todos los rï¿½tulos que corresponden a la misma rama. Usar foldrANR.

-}

--a
data ArbolNRot a b = ArbolNRotVacio | Nodo a | ArbolNRotNoVacio b [ArbolNRot a b] deriving (Eq, Show)

--b
--foldr :: (a -> b -> b) -> b -> [a] -> b

{-foldrANR :: (a -> c -> c) -> (b -> c) -> ArbolNRot a b -> c
foldrANR f g arb =  foldr f (g ()) ()-}



rotulosRamas:: (Num b) => ArbolNRot a b -> [b]
rotulosRamas ArbolNRotVacio = []
rotulosRamas (Nodo a) = []
{-
rotulosRamas (ArbolNRotNoVacio b (x:xs)) = foldrANR (+)
-}


{-rotulosRamas  ArbolNRotVacio = []
rotulosRamas  Nodo a = []
rotulosRamas (ArbolNRotNoVacio a b) = b + (rotulosRamas( ArbolNRot a b))-}

--4)
-- map :: (a -> b) -> [a] -> [b]
-- map map :: [a -> b] -> [[a] -> [b]]
--[length, head] :: [[Int] -> Int]
-- map map [length, head] :: [[[Int]] -> [Int]]


-- tipo de map map
-- map :: (a -> b) -> [a] -> [b]
-- map :: (y -> z) -> [y] -> [z]

-- map map  { a/(y -> z) , b/ [y] -> [z]} ::  [(y -> z)] -> [[y] -> [z]]  }

-- tipo map map [length, head]
-- map map :: [(y -> z)] -> [[y] -> [z]] 
-- [length, head] :: [[Int] -> Int]

--  { y/ [Int]  ,z /Int} ::  [[[Int]] -> [Int]] }
