{-# LANGUAGE LambdaCase #-}
module Tree where

import ClassyPrelude

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving Show

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f = \ case
  Leaf -> Leaf
  Node left x right -> Node (mapTree f left) (f x) (mapTree f right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f y = \ case
  Leaf -> y
  Node left x right -> foldTree f (f x (foldTree f y right)) left
