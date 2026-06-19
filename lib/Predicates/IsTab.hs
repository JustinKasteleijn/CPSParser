{-# LANGUAGE InstanceSigs #-}

module Predicates.IsTab where

class IsTab s where
  isTab :: s -> Bool

instance IsTab Char where
  isTab :: Char -> Bool
  isTab = (==) '\t'
