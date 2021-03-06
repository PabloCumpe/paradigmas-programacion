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
    f (a, b) c
      | a > b = []
      | a + b == c = (a, b) : f (a + 1, b - 1) c
      | otherwise = f (a + 1, b - 1) c

--b
paresUsandoMonada num = enumFromTo 1 (div num 2) >>= (\x -> [(x,num-x)])


{-
2)		Evaluar la siguiente expresión:

		take 2 [x+1 | x <- [2,5..], even x]

		utilizando modo de evaluación eager por un lado y normal order por otro, explicando con mayor detalle el modo de evaluación normal order. Considerar la evaluación de even como atómica (que reduce en un solo paso al resultado final).
-}

--EAGER
--  take 2 [x+1 | x <- [2,5..], even x]
--  take 2 [x+1 | x <- [2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,..], even x]
-- no termina por ser una lista infinita
--
--
--  LAZY
--     take 2 [x+1 | x <- [2,5..], even x]
--     take 2 [2+1 | 2 <- [2,5..], true]
--     take 2 [ 3  | x <- [5..]  , even x]
-- [3] take 1 [x+1 | 5 <- [5..]  , false]
-- [3] take 1 [8+1 | 8 <- [8..]  , true]
-- [3] take 1 [  9 | 8 <- [8..]  , even x]
-- [3,9]




{-
3)	i)	Definir el tipo de datos ArbolNRot, que representa un ï¿½rbol n-ario donde los nodos poseen un valor de un tipo dado, y donde los arcos (rï¿½tulos) que unen un nodo con cada subï¿½rbol tambiï¿½n poseen un valor de eventualmente otro tipo dado. Considerar que el tipo puede ser paramï¿½trico.
	ii)	Definir la funciï¿½n de orden superior foldrANR que dado un ï¿½rbol ArbolNRot en forma anï¿½loga a la funciï¿½n foldr de las listas (recursiï¿½n de pila), y que posea parï¿½metros adecuados que apliquen a este tipo de datos.
	iii)	Definir la funciï¿½n rotulosRamas, que dado un ï¿½rbol ArbolNRot con nodos de un tipo arbitrario y rï¿½tulos enteros, retorne una lista de enteros donde cada elemento entero se asocia a cada rama del ï¿½rbol y corresponde a la suma de todos los rï¿½tulos que corresponden a la misma rama. Usar foldrANR.

-}

--a
data ArbolNRot a b = ArbolVacio | Hoja a | Nodo a b [ArbolNRot a b]  deriving (Show, Read, Eq)

--b

foldrANR ::  (b -> acc -> acc) -> acc -> ArbolNRot a b -> [acc]
foldrANR f z (Hoja a) = [z]
foldrANR f z ArbolVacio = [z]
foldrANR f z (Nodo a b []) = [f b z]
foldrANR f z (Nodo a b ns) = [f b y | n <- ns, y <- foldrANR f z n]

--c



rotulosRamas arb = foldrANR (+) 0


t =  Nodo "1" 1 [
        Nodo "2" 2 [
           Nodo "4" 3 [
                Hoja "6"
                ]
            ],
        Nodo "3" 1 [
            Hoja "5"
            ]
     ]

--4)



-- map :: (a -> b) -> [a] -> [b]
-- map :: (y -> z) -> [y] -> [z]

-- map map  [ a/(y -> z) , b/ [y] -> [z] ] ::  [(y -> z)] -> [[y] -> [z]]

-- map map :: [(y -> z)] -> [[y] -> [z]]
-- [length, head] :: [[Int] -> Int]

--  map map [length, head]  [ y/ [Int]  ,z /Int] ::  [[[Int]] -> [Int]]
