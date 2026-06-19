{-# LANGUAGE InstanceSigs #-}

module Predicates.IsDigit where

import qualified Data.Char as C

class IsDigit s where
  isDigit :: s -> Bool
  toDigit :: (Num n) => s -> n

instance IsDigit Char where
  isDigit :: Char -> Bool
  isDigit = C.isDigit

  toDigit :: (Num n) => Char -> n
  toDigit c = fromIntegral (fromEnum c - fromEnum '0')
