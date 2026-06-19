{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module CustomTestLib where

import Test.Syd
import Parser
import Position

infix 1 ==>
infix ==?

class AssertSuccess e s a expected | a s -> expected where
  (==>) :: Either (e, s) (a, s) -> expected -> Expectation

class AssertFailure e s a expected | e s -> expected where
  (==?) :: Either (e, s) (a, s) -> expected -> Expectation

instance (Eq e, Show e, Eq s, Show s, Eq a, Show a)
  => AssertSuccess e s a (a, s) where
    result ==> expected = result `shouldBe` Right expected

instance (Eq e, Show e, Eq s, Show s, Eq a, Show a)
  => AssertFailure e s a (e, s) where
    result ==? expected = result `shouldBe` Left expected

runTestParser :: Parser String String a -> String -> Either (String, String) (a, String)
runTestParser = runParser

runPositionalParser :: Parser (Positional String) String a -> Positional String -> Either (String, Positional String) (a, Positional String)
runPositionalParser = runParser
