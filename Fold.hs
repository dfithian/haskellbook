module Fold where

import ClassyPrelude
import qualified Data.List as L

myFoldL :: (a -> b -> a) -> a -> [b] -> a
myFoldL _ x [] = x
myFoldL f x (y:ys) = myFoldL f (f x y) ys

myFoldR :: (b -> a -> a) -> a -> [b] -> a
myFoldR _ x [] = x
myFoldR f x (y:ys) = f y (myFoldR f x ys)

myScanL :: (a -> b -> a) -> a -> [b] -> [a]
myScanL f x ys =
  let rest = case ys of
        y:ys' -> myScanL f (f x y) ys'
        [] -> []
  in x:rest

myScanR :: (b -> a -> a) -> a -> [b] -> [a]
myScanR _ x [] = [x]
myScanR f x (y:ys) = f y x:myScanR f x ys

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:myIterate f (f x)

myUnfoldR :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldR f y = case f y of
  Just (x, y') -> x:myUnfoldR f y'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldR (\ x -> Just (x, f x))
