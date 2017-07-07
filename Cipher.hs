module Cipher where

import ClassyPrelude
import Data.Char (chr, ord)
import Data.List (cycle)

newtype Caesar = Caesar { unCaesar :: String } deriving (Eq, Ord, Show, Monoid, Semigroup)
newtype Vigenere = Vigenere { unVigenere :: String } deriving (Eq, Ord, Show, Monoid, Semigroup)

encodeCaesar :: Int -> String -> Caesar
encodeCaesar n = \ case
  (x:xs) ->
    let c = case ord x + n of
          inBounds | inBounds < ord 'a' + 26 -> inBounds
          outBounds -> (+ ord 'a') $ outBounds `mod` (ord 'a' + 26)
    in (Caesar . singleton . chr $ c) <> encodeCaesar n xs
  _ -> mempty

decodeCaesar :: Int -> Caesar -> String
decodeCaesar n = unCaesar . encodeCaesar (26 - n) . unCaesar

zipWords :: String -> String -> [Maybe (Char, Char)]
zipWords (cycle -> shift) = go shift
  where
    go shift' word = case (shift', word) of
      (xs, ' ':ys) -> Nothing:go xs ys
      (x:xs, y:ys) -> Just (x,y):go xs ys
      _ -> mempty

encodeVigenere :: String -> String -> Vigenere
encodeVigenere shift = Vigenere . go . zipWords shift
  where
    go = \ case
      Nothing:rest -> ' ':go rest
      Just (x, y):rest -> unCaesar (encodeCaesar (ord y - ord 'a') (singleton x)) <> go rest
      _ -> mempty

flipChars :: String -> String
flipChars = \ case
  x:rest -> chr (ord 'a' + (26 - (ord x - ord 'a'))):flipChars rest
  _ -> mempty

decodeVigenere :: String -> Vigenere -> String
decodeVigenere (flipChars -> shift) = unVigenere . encodeVigenere shift . unVigenere
