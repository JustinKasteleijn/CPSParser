{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Parsable where

import qualified Data.Text as T
import           Parser    (Parser)
import           Position  (Position (..), Positional (..))

class Parsable s where
  type Elem s
  uncons :: s -> Maybe (Elem s, s)

instance Parsable [a] where
  type Elem [a] = a

  uncons :: [a] -> Maybe (Elem [a], [a])
  uncons []     = Nothing
  uncons (x:xs) = Just (x, xs)

instance Parsable T.Text where
  type Elem T.Text = Char

  uncons :: T.Text -> Maybe (Elem T.Text, T.Text)
  uncons = T.uncons

instance (Parsable s, Elem s ~ Char) => Parsable (Positional s) where
  type Elem (Positional s) = Char

  uncons :: Positional s -> Maybe (Elem (Positional s), Positional s)
  uncons (Positional (Position l c) stream) =
    case uncons stream of
      Nothing -> Nothing
      Just (ch, nextStream) ->
        let nextPos = case ch of
              '\n' -> Position (l + 1) 1
              '\t' -> Position l ((c + 4) - ((c - 1) `mod` 4))
              _    -> Position l (c + 1)
         in Just (ch, Positional nextPos nextStream)
