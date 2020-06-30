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
{-pares :: (Ord a, Eq a, Num a) => a -> [(a, a)]
pares 0 = []
pares 1 = []
pares num = obtenerPares (1, num - 1) num
obtenerPares :: (Ord a, Eq a, Num a) => (a, a) -> a -> [(a, a)]
obtenerPares (a, b) c
  | a > b = []
  | a + b == c = (a, b) : obtenerPares (a + 1, b - 1) c
  | otherwise = obtenerPares (a + 1, b - 1) c-}

pares :: (Ord a, Eq a, Num a) => a -> [(a, a)]
pares 0 = []
pares 1 = []
pares num = f (1, num - 1) num
    where
    f :: (Ord a, Eq a, Num a) => (a, a) -> a -> [(a, a)]
    f (a, b) c
      | a > b = []
      | a + b == c = (a, b) : f (a + 1, b - 1) c
      | otherwise = f (a + 1, b - 1) c

--b
paresUsandoMonada num = enumFromTo 1 (div num 2) >>= (\x -> [(x,num-x)])

{-
3)	i)	Definir el tipo de datos ArbolNRot, que representa un ï¿½rbol n-ario donde los nodos poseen un valor de un tipo dado, y donde los arcos (rï¿½tulos) que unen un nodo con cada subï¿½rbol tambiï¿½n poseen un valor de eventualmente otro tipo dado. Considerar que el tipo puede ser paramï¿½trico.
	ii)	Definir la funciï¿½n de orden superior foldrANR que dado un ï¿½rbol ArbolNRot en forma anï¿½loga a la funciï¿½n foldr de las listas (recursiï¿½n de pila), y que posea parï¿½metros adecuados que apliquen a este tipo de datos.
	iii)	Definir la funciï¿½n rotulosRamas, que dado un ï¿½rbol ArbolNRot con nodos de un tipo arbitrario y rï¿½tulos enteros, retorne una lista de enteros donde cada elemento entero se asocia a cada rama del ï¿½rbol y corresponde a la suma de todos los rï¿½tulos que corresponden a la misma rama. Usar foldrANR.

-}

--a
data ArbolNRot a b = ArbolVacio | Hoja a | Nodo a b [ArbolNRot a b] deriving (Eq, Show)

--b
--foldr :: (a -> b -> b) -> b -> [a] -> b
{-foldrANR :: (a -> c -> c) -> (b -> c) -> ArbolNRot a b -> c
foldrANR f g arb = f g ()-}


data Tree a b = EmptyTree | Leaf a | Node a b [Tree a b]
            deriving (Show, Read, Eq)

t =  Node "goal" 1 [
        Node "c1" 2 [
           Node "c3" 3 [
                Leaf "c5"
              ]
            ],
        Node "c2" 5 [
            Leaf "c4" 
            ]
     ]

sumTree :: (Num b) => Tree a b -> b
sumTree EmptyTree = 0
sumTree (Node _ value []) = value
sumTree (Node _ value [x]) = value + sumTree x
sumTree (Node name value (x:xs)) = value + sumTree x + sumTree (Node name 0 xs)


rotulosRamas :: (Num b) => Tree a b -> [b]
rotulosRamas EmptyTree = []
rotulosRamas  (Node a b []) = []
rotulosRamas (Node a value (x:xs)) = sumTree (Node a value (x:xs)): sumTree x : rotulosRamas (Node a 0 xs)


depth :: Tree a b -> Int
depth EmptyTree = 0
depth (Node _ _ []) = 1
depth (Node _ _ [x]) = 1 + depth x
depth (Node n v (x:xs)) = 1 + depth (Node n v xs)



--4)



-- tipo de map map
-- map :: (a -> b) -> [a] -> [b]
-- map :: (y -> z) -> [y] -> [z]

-- map map  [ a/(y -> z) , b/ [y] -> [z] ] ::  [(y -> z)] -> [[y] -> [z]]  }

-- tipo map map [length, head]
-- map map :: [(y -> z)] -> [[y] -> [z]]
-- [length, head] :: [[Int] -> Int]

--  [ y/ [Int]  ,z /Int] ::  [[[Int]] -> [Int]]
