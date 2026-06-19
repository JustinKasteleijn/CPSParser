{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Parsable where

import qualified Data.Text            as T
import           Parser               (Parser)
import           Position             (Position (..), Positional (..))
import           Predicates.IsNewline (IsNewline (..))
import           Predicates.IsTab     (IsTab (..))

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

instance (Parsable s, IsNewline (Elem s), IsTab (Elem s)) => Parsable (Positional s) where
  type Elem (Positional s) = Elem s

  uncons :: Positional s -> Maybe (Elem (Positional s), Positional s)
  uncons (Positional (Position l c) stream) =
    case uncons stream of
      Nothing -> Nothing
      Just (ch, nextStream) ->
        let nextPos = if | isNewline ch -> Position (l + 1) 0
                         | isTab ch     -> Position l ((c + 4) - ((c - 1) `mod` 4))
                         | otherwise    -> Position l (c + 1)
         in Just (ch, Positional nextPos nextStream)
