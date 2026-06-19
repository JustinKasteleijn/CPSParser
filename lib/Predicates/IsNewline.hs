{-# LANGUAGE InstanceSigs #-}

module Predicates.IsNewline where

import qualified Data.Text as T

class IsNewline s where
  isNewline :: s -> Bool

instance IsNewline Char where
  isNewline :: Char -> Bool
  isNewline = (==) '\n'
