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
data Poste =  Origen | Destino | Auxiliar deriving Show
type Movimiento = (Poste, Poste)

hanoi :: Int -> [Movimiento]
hanoi n = hanoi2 n Origen Destino Auxiliar
hanoi2 :: Int -> Poste -> Poste -> Poste -> [Movimiento]
hanoi2 n o d a
  | n == 0 = []
  | n > 0 = hanoi2 (n - 1) o a d ++ [(o, d)] ++ hanoi2 (n - 1) a d o

 ----------------------------------------------------------
 ---2
data Extension a = Empty | Justt a deriving Show

---3

data Grupo = O | A | B | AB deriving (Eq, Show)
type Factor = Bool
data TipoDeSangre = TS Grupo Factor deriving (Eq, Show)
type TipoDeSangre2 = (Grupo,Factor)

donag :: Grupo -> Grupo -> Bool
donag O x = True
donag A x = elem x [A,AB]
donag B x = elem x [B , AB]
donag AB x = (x == AB)

dona :: TipoDeSangre -> TipoDeSangre -> Bool
dona (TS g1 f1) (TS g2 f2) = (f1 == f2) && donag g1 g2

--------4
-- Cera constante constructora
-- funciones constructoras no tienen definicion
data Nat = Cero | Suc Nat deriving (Eq,Show)
one = Suc Cero
two = Suc one
three = Suc two
four = Suc three
sumarNats :: Nat -> Nat -> Nat
sumarNats Cero x = x
sumarNats (Suc n) y = sumarNats n (Suc y)

-- four es igual a suc(three), cuando se pasa un nat te da el suc con el numero anterior
restarNats :: Nat -> Nat -> Nat
restarNats x Cero = x
restarNats (Suc x) (Suc y) = restarNats x y

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = f x 1
  where
  f Cero c = c
  f (Suc x) c = f x (c+1)