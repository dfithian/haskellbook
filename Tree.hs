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

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f y = case f y of
  Just (x1, y', x2) -> Node (unfoldTree f x1) y' (unfoldTree f x2)
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild upper =
  let f = \ case
        u | u == upper -> Nothing
        x -> Just (x + 1, x, x + 1)
  in unfoldTree f 0
