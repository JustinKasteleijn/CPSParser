{-# LANGUAGE InstanceSigs #-}

module Predicates.IsSpace where

import qualified Data.Char as C

class IsSpace s where
  isSpace :: s -> Bool

instance IsSpace Char where
  isSpace :: Char -> Bool
  isSpace = C.isSpace
