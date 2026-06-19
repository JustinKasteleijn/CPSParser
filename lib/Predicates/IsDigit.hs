{-# LANGUAGE InstanceSigs #-}

module Predicates.IsDigit where

import qualified Data.Char as C

class IsDigit s where
  isDigit :: s -> Bool

instance IsDigit Char where
  isDigit :: Char -> Bool
  isDigit = C.isDigit
