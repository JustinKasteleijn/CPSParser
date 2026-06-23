{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Combinators where

import           Control.Applicative  (empty, many, optional, some, (<|>))
import           Control.Monad        (void)
import           Data.Char            (digitToInt)
import           Data.Int             (Int16, Int32, Int64, Int8)
import           Data.List            (foldl')
import           Data.Word            (Word16, Word32, Word64, Word8)
import           GHC.List             (foldr')
import           Parsable             (Parsable (Elem), uncons)
import           Parser               (Parser (..), failParser)
import           ParserError          (ParserError (expectedButGot, unexpectedEOF, unexpectedError))
import           Predicates.IsAlpha   (IsAlpha (..))
import           Predicates.IsDigit   (IsDigit (..))
import           Predicates.IsMinus   (IsMinus (..))
import           Predicates.IsNewline (IsNewline (..))
import           Predicates.IsSpace   (IsSpace (..))
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

char :: (Parsable s, ParserError s e, Elem s ~ Char) => Char -> Parser s e (Elem s)
char c = satisfy (== c) [c] show

string :: (Parsable s, ParserError s e, Elem s ~ Char) => String -> Parser s e [Elem s]
string = mapM char

digit :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s)) => Parser s e (Elem s)
digit = satisfy isDigit "digit '0..9'" show

digits0 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s)) => Parser s e [Elem s]
digits0 = many digit

digits1 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s)) => Parser s e [Elem s]
digits1 = some digit

newline :: (Parsable s, ParserError s e, IsNewline (Elem s), Show (Elem s)) => Parser s e ()
newline = void $ satisfy isNewline "\n" show

lines1 :: (Parsable s, ParserError s e, IsNewline (Elem s), Show (Elem s)) => Parser s e a -> Parser s e [a]
lines1 = sepBy1 newline

lines0 :: (Parsable s, ParserError s e, IsNewline (Elem s), Show (Elem s)) => Parser s e a -> Parser s e [a]
lines0 = sepBy0 newline

tab :: (Parsable s, ParserError s e, IsTab (Elem s), Show (Elem s)) => Parser s e (Elem s)
tab = satisfy isTab "\t" show

tab0 :: (Parsable s, ParserError s e, IsTab (Elem s), Show (Elem s)) => Parser s e [Elem s]
tab0 = many tab

tab1 :: (Parsable s, ParserError s e, IsTab (Elem s), Show (Elem s)) => Parser s e [Elem s]
tab1 = some tab

alpha :: (Parsable s, ParserError s e, IsAlpha (Elem s), Show (Elem s)) => Parser s e (Elem s)
alpha = satisfy isAlpha "alpha 'a..z|A..Z'" show

alpha0 :: (Parsable s, ParserError s e, IsAlpha (Elem s), Show (Elem s)) => Parser s e [Elem s]
alpha0 = many alpha

alpha1 :: (Parsable s, ParserError s e, IsAlpha (Elem s), Show (Elem s)) => Parser s e [Elem s]
alpha1 = some alpha

sepBy1 :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e sep -> Parser s e a -> Parser s e [a]
sepBy1 psep parser = (:) <$> parser <*> many (psep >> parser)

sepBy0 :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e sep -> Parser s e a -> Parser s e [a]
sepBy0 psep parser = sepBy1 psep parser <|> pure []

sepBy1Trailing :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e sep -> Parser s e (Elem s) -> Parser s e [Elem s]
sepBy1Trailing psep parser = do
  result <- sepBy1 psep parser
  _      <- optional psep
  pure result

sepBy0Trailing :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e sep -> Parser s e (Elem s) -> Parser s e [Elem s]
sepBy0Trailing psep parser = sepBy1Trailing psep parser <|> pure []

between :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e l -> Parser s e r -> Parser s e a -> Parser s e a
between pLeft pRight parser = pLeft *> parser <* pRight

whitespace :: (Parsable s, ParserError s e, IsSpace (Elem s), Show (Elem s)) => Parser s e ()
whitespace = void $ satisfy isSpace "whitespace" show

whitespace1 :: (Parsable s, ParserError s e, IsSpace (Elem s), Show (Elem s)) => Parser s e ()
whitespace1 = void $ some whitespace

whitespace0 :: (Parsable s, ParserError s e, IsSpace (Elem s), Show (Elem s)) => Parser s e ()
whitespace0 = void $ many whitespace

eof :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e ()
eof = Parser $ \stream success failure ->
        case uncons stream of
          Nothing     -> success () stream
          Just (x,xs) -> failure (expectedButGot (show x) "eof" xs) xs

choice :: (ParserError s e) => [Parser s e a] -> Parser s e a
choice = foldr' (<|>) empty

try :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e a -> Parser s e a
try (Parser p) =
  Parser $ \stream success failure ->
    let retry err _ = failure err stream
      in p stream success retry

peek :: Parser s e a -> Parser s e a
peek (Parser p) =
  Parser $ \stream success failure ->
      let success' val _ = success val stream
       in p stream success' failure

notFollowedBy :: (Parsable s, ParserError s e, Show (Elem s)) => Parser s e a -> Parser s e ()
notFollowedBy (Parser p) =
  Parser $ \stream success failure ->
    let success' val _ = failure (unexpectedError "Parse action 'notFollowedBY' succeeded but wat forbidden" stream) stream
        failure' val = success ()
      in p stream success' failure'
-----------------------------------------------------------------
-- Numbers
-----------------------------------------------------------------

int :: (Parsable s, ParserError s e, IsDigit (Elem s), IsMinus (Elem s), Show (Elem s)) => Parser s e Integer
int = signedInt

nat :: (Parsable s, ParserError s e, IsDigit (Elem s) , Show (Elem s)) => Parser s e Integer
nat = unsignedInt

unsignedInt :: (Parsable s, ParserError s e, IsDigit (Elem s),Show (Elem s), Num n) => Parser s e n
unsignedInt = foldl' (\acc d -> acc * 10 + toDigit d) 0 <$> digits1

signedInt :: (Parsable s, ParserError s e, IsDigit (Elem s), IsMinus (Elem s), Show (Elem s), Num n) => Parser s e n
signedInt = do
   f <- option id (negate <$ satisfy isMinus "-" show)
   f <$> unsignedInt
  where
    option def p = p <|> pure def

parseBounded :: forall s e n.
                (Parsable s, ParserError s e, IsDigit (Elem s), IsMinus (Elem s), Show (Elem s), Integral n, Bounded n, Show n)
                  => Parser s e n
parseBounded = do
  val <- signedInt :: Parser s e Word64
  let targetMin = fromIntegral (minBound :: n) :: Word64
      targetMax = fromIntegral (maxBound :: n) :: Word64

  if val >= targetMin && val <= targetMax
     then pure (fromIntegral val)
     else failParser $
                expectedButGot
                    (show val)
                    ("a value between " ++ show (minBound :: n) ++ " and " ++ show (maxBound :: n))

parseUnsignedBounded :: forall s e n. (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s), Integral n, Bounded n, Show n)
                     => Parser s e n
parseUnsignedBounded = do
  val <- unsignedInt :: Parser s e Word64
  let targetMin = fromIntegral (minBound :: n) :: Word64
      targetMax = fromIntegral (maxBound :: n) :: Word64
  if val >= targetMin && val <= targetMax
     then pure (fromIntegral val)
     else failParser $
               expectedButGot
                  (show val)
                  ("a value between " ++ show (minBound :: n) ++ " and " ++ show (maxBound :: n))

u8 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s))
   => Parser s e Word8
u8 = parseUnsignedBounded

u16 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s))
   => Parser s e Word16
u16 = parseUnsignedBounded

u32 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s))
   => Parser s e Word32
u32 = parseUnsignedBounded

u64 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s))
   => Parser s e Word64
u64 = parseUnsignedBounded

i8 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s), IsMinus (Elem s))
   => Parser s e Int8
i8 = parseBounded

i16 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s), IsMinus (Elem s))
   => Parser s e Int16
i16 = parseBounded

i32 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s), IsMinus (Elem s))
   => Parser s e Int32
i32 = parseBounded

i64 :: (Parsable s, ParserError s e, IsDigit (Elem s), Show (Elem s), IsMinus (Elem s))
   => Parser s e Int64
i64 = parseBounded
