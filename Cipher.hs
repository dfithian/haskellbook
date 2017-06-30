module Cipher where

import ClassyPrelude
import Data.Char (chr, ord)

newtype Caesar = Caesar { unCaesar :: String } deriving (Eq, Ord, Show, Monoid, Semigroup)

encode :: Int -> String -> Caesar
encode n = \ case
  (x:xs) ->
    let c = case ord x + n of
          inBounds | inBounds < ord 'a' + 26 -> inBounds
          outBounds -> (+ ord 'a') $ outBounds `mod` (ord 'a' + 26)
    in (Caesar . singleton . chr $ c) <> encode n xs
  _ -> mempty

decode :: Int -> Caesar -> String
decode n = unCaesar . encode (26 - n) . unCaesar
