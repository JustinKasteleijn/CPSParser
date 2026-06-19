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
