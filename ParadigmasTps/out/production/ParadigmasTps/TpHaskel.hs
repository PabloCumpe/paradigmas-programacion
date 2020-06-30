-- comentario
{-# LANGUAGE BlockArguments #-}

import Prelude hiding (max, min)
import System.IO

-- ghci
-- :l archivo
-- :r -> compilar
-- :t funcion -> ver definicion de funcion
data Tree a b
  = Leaf a b
  | Node a [Tree a b]
  deriving (Show)

getValueFromNode :: (Num b) => Tree a b -> b
getValueFromNode (Leaf _ b) = b
getValueFromNode (Node _ []) = 0
getValueFromNode (Node a (x:xs)) = getValueFromNode x + getValueFromNode (Node a xs)

exampleTree :: Tree Char Int
exampleTree = Node 'A' [Leaf 'B' 1, Leaf 'C' 2, Leaf 'D' 3]

exampleTreeWithMultipleNodes :: Tree Char Int
exampleTreeWithMultipleNodes =
  Node
    'A'
    [ Node 'B' [Leaf 'E' 1, Leaf 'H' 2, Leaf 'J' 3]
    , Node 'C' [Leaf 'F' 1, Leaf 'I' 2, Leaf 'L' 3]
    , Node 'D' [Leaf 'G' 1, Leaf 'J' 2, Leaf 'M' 3]
    ]