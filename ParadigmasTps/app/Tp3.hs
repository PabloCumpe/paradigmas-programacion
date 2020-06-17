-- comentario
{-# LANGUAGE BlockArguments #-}

import Prelude hiding (max, min)
import System.IO

-- ghci
-- :l archivo
-- :r -> compilar
-- :t funcion -> ver definicion de funcion
-------------------------------------------
-- constantes constructoras en mayusculas
--1

{-
esCerrada :: [a] -> ([a] -> a) -> Bool
-}


--2
curry :: ((a,b) -> c )-> a -> b -> c
--curry f x y = f (x,y)
curry f = (\x -> (\y-> f (x,y)))

while :: (a -> Bool) -> (a -> a) -> a -> a
while e f a =
  if e a
    then while e f (f a)
    else a

