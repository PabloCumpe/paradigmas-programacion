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
data Color = Azul Int | Rojo | Verde | Amarillo | Violeta  | Rosa | Naranja deriving Show