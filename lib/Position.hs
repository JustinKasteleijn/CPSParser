{-# LANGUAGE InstanceSigs #-}

module Position (
    Position(..),
    Positional(..),
    startPositional,
    mkPositional
) where

data Position = Position
  { line :: !Int,
    col  :: !Int
  }
  deriving (Eq)

instance Semigroup Position where
  (<>) :: Position -> Position -> Position
  Position l c <> Position dl dc
    | dl > 0    = Position (l + dl) dc
    | otherwise = Position l (c + dl)

instance Monoid Position where
  mempty :: Position
  mempty = Position 0 0

instance Show Position where
  show :: Position -> String
  show (Position line col) = show line ++ ":" ++ show col

data Positional s = Positional {
    getPos    :: !Position,
    getStream :: !s
  }
  deriving (Eq)

instance (Show s) => Show (Positional s) where
  show :: Positional s -> String
  show (Positional pos stream) = show pos ++ " from: " ++ show stream

startPositional :: s -> Positional s
startPositional stream = Positional {
    getPos = mempty,
    getStream = stream
  }

mkPositional :: s -> Position -> Positional s
mkPositional stream pos = Positional {
    getPos = pos,
    getStream  = stream
  }
