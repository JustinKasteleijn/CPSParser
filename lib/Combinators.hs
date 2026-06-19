{-# LANGUAGE FlexibleContexts #-}

module Combinators where

import           Control.Applicative  (many, some)
import           Parsable             (Parsable (Elem), uncons)
import           Parser               (Parser (..))
import           ParserError          (ParserError (expectedButGot, unexpectedEOF))
import           Predicates.IsAlpha   (IsAlpha (..))
import           Predicates.IsDigit   (IsDigit (..))
import           Predicates.IsNewline (IsNewline (..))
import           Predicates.IsTab     (IsTab (..))

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

newline :: (Parsable s, ParserError s e, IsNewline (Elem s), Show (Elem s)) => Parser s e (Elem s)
newline = satisfy isNewline "'\n'" show

newlines0 :: (Parsable s, ParserError s e, IsNewline (Elem s), Show (Elem s)) => Parser s e [Elem s]
newlines0 = many newline

newlines1 :: (Parsable s, ParserError s e, IsNewline (Elem s), Show (Elem s)) => Parser s e [Elem s]
newlines1 = some newline

tab :: (Parsable s, ParserError s e, IsTab (Elem s), Show (Elem s)) => Parser s e (Elem s)
tab = satisfy isTab "'\t'" show

tab0 :: (Parsable s, ParserError s e, IsTab (Elem s), Show (Elem s)) => Parser s e [Elem s]
tab0 = many tab

tab1 :: (Parsable s, ParserError s e, IsTab (Elem s), Show (Elem s)) => Parser s e [Elem s]
tab1 = some tab

alpha :: (Parsable s, ParserError s e, IsAlpha (Elem s), Show (Elem s)) => Parser s e (Elem s)
alpha = satisfy isAlpha "'alpha 'a..z|A..Z'" show

alpha0 :: (Parsable s, ParserError s e, IsAlpha (Elem s), Show (Elem s)) => Parser s e [Elem s]
alpha0 = many alpha

alpha1 :: (Parsable s, ParserError s e, IsAlpha (Elem s), Show (Elem s)) => Parser s e [Elem s]
alpha1 = some alpha

sepBy1 :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e sep -> Parser s e (Elem s) -> Parser s e [Elem s]
sepBy1 psep parser = (:) <$> parser <*> many (psep >> parser)

sepBy0 :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e sep -> Parser s e (Elem s) -> Parser s e [Elem s]
sepBy0 psep parser = sepBy1 psep parser <|> pure []
