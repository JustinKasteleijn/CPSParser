{-# LANGUAGE InstanceSigs #-}

module Predicates.IsMinus where

class IsMinus s where
  isMinus :: s -> Bool

instance IsMinus Char where
  isMinus :: Char -> Bool
  isMinus = (==) '-'
