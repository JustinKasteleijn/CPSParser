{-# LANGUAGE InstanceSigs #-}

module Predicates.IsAlpha where

import qualified Data.Char as C

class IsAlpha s where
  isAlpha :: s -> Bool

instance IsAlpha Char where
  isAlpha :: Char -> Bool
  isAlpha = C.isAlpha
