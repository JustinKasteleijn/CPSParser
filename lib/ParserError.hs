{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ParserError (
    ParserError (..)
) where

import           Position      (Position (..), Positional (..))
import           Text.Read.Lex (expect)

class ParserError s e where
  emptyError :: s -> e
  unexpectedEOF :: s -> e
  expectedButGot :: String -> String -> s -> e

instance ParserError (Positional s) String where
  emptyError :: Positional s -> String
  emptyError (Positional pos _)
     = show pos ++ " No alternative found"

  unexpectedEOF :: Positional s -> String
  unexpectedEOF (Positional pos _)
     = show pos ++ " unexpected EOF"

  expectedButGot :: String -> String -> Positional s -> String
  expectedButGot actual expected (Positional pos _)
    = show pos ++ " Parser expected '" ++ expected ++ "' but got " ++ actual

instance ParserError String String where
  emptyError :: String -> String
  emptyError _ = "No alternative found"

  unexpectedEOF :: String -> String
  unexpectedEOF _ = "unexpected EOF"

  expectedButGot :: String -> String -> String -> String
  expectedButGot actual expected _ = "Parser expected '" ++ expected ++ "' but got " ++ actual
