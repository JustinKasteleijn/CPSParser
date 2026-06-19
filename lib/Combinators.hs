{-# LANGUAGE FlexibleContexts #-}

module Combinators where

import           Control.Applicative (many, some)
import           Parsable            (Parsable (Elem), uncons)
import           Parser              (Parser (..))
import           ParserError         (ParserError (expectedButGot, unexpectedEOF))
import           Predicates.IsDigit  (IsDigit (..))

item :: (Parsable s, ParserError s e) => Parser s e (Elem s)
item = Parser $ \stream success failure ->
          case uncons stream of
             Just (x,xs) -> success x xs
             Nothing     -> failure (unexpectedEOF stream) stream

satisfy :: (Parsable s, ParserError s e) => (Elem s -> Bool) -> String -> (Elem s -> String) -> Parser s e (Elem s)
satisfy pred expected formatActual  =
  Parser $ \stream success failure ->
    case uncons stream of
      Nothing      -> failure (unexpectedEOF stream) stream
      Just (x, xs) -> if pred x
                         then success x xs
                         else failure (expectedButGot (formatActual x) expected stream) stream

digit :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s)) => Parser s e (Elem s)
digit = satisfy isDigit "digit '0..9'" show

digit0 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s)) => Parser s e [Elem s]
digit0 = many digit

digit1 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s)) => Parser s e [Elem s]
digit1 = some digit
