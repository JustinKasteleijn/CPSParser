{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE RankNTypes    #-}

module Parser (
    Parser(..),
    runParser,
    failParser
) where
import           Control.Applicative (Alternative (..))
import           ParserError         (ParserError (emptyError))

newtype Parser s e a
  = Parser {
      parse
        :: forall r
        .  s
        -> (a -> s -> r)
        -> (e -> s -> r)
        -> r
    }
    deriving (Functor)

instance Applicative (Parser s e) where
  pure :: a -> Parser s e a
  pure x = Parser $ \stream success failure -> success x stream

  (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
  pf <*> px = pf >>= \f -> px >>= \x -> pure (f x)

instance Monad (Parser s e) where
  (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
  (Parser parser) >>= f =
    Parser $ \stream success failure ->
      parser
        stream
        (\x stream' -> parse (f x) stream' success failure)
        failure

instance (ParserError s e) => Alternative (Parser s e) where
  empty :: Parser s e a
  empty = Parser $ \stream _ failure -> failure (emptyError stream) stream

  (<|>) :: Parser s e a -> Parser s e a -> Parser s e a
  px <|> py = Parser $ \stream success failure ->
     parse px stream success (\_ _ -> parse py stream success failure)

failParser :: (s -> e) -> Parser s e a
failParser errBuilder = Parser $ \stream _ failure -> failure (errBuilder stream) stream

runParser :: Parser s e a -> s -> Either (e, s) (a, s)
runParser (Parser p) stream = let success val rest = Right (val, rest)
                                  failure err rest = Left (err, rest)
                               in p stream success failure
