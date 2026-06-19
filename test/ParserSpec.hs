module ParserSpec (spec) where

import           Combinators
import           CustomTestLib
import           Position
import           Test.Syd

spec :: Spec
spec = do
  describe "Test for parser generic properties" $ do
    testColPosition


testColPosition :: Spec
testColPosition = describe "Test col position" $ do
  it "Increases the position when single character consumed" $ do
    runPositionalParser item (startPositional "hello") ==> ('h', mkPositional "ello" (Position 0 1))
  it "Increases the position by 2 when parser is ran twice" $ do
    runPositionalParser (item *> item) (startPositional "hello") ==> ('e', mkPositional "llo" (Position 0 2))
  it "Errors when EOF is hit in the middle of parsing" $ do
    let testPos = Position 0 2
    runPositionalParser (item *> item *> item) (startPositional "he") ==? (show testPos ++ " unexpected EOF", mkPositional "" testPos)
  it "increases the position by 1 when single newline consumed" $ do
    runPositionalParser newline (startPositional "\n") ==> ((), mkPositional "" (Position 1 0))
  it "increases the position by 2 when 2 newline characters are consumed" $ do
    runPositionalParser (newline *> newline) (startPositional "\n\n") ==> ((), mkPositional "" (Position 2 0))
  it "Intergration test for a string with newlines and character" $ do
    runPositionalParser ((,) <$> alpha1 <*> (newline *> alpha1)) (startPositional "some\nstring") ==> (("some", "string"), mkPositional "" (Position 1 6))
