module CombinatorsSpec (spec) where

import           Combinators
import           CustomTestLib
import           ParserError
import           Test.Syd


spec :: Spec
spec = do
  describe "Test for parser combinators" $ do
    testItemCombinator
    testDigitCombinator


testItemCombinator :: Spec
testItemCombinator = describe "Item Combinator" $ do
  it "Consumes a single character from a populated stream" $ do
    runTestParser item "hello" ==> ('h', "ello")
  it "Fails gracefully with an EOF error when given an empty stream" $ do
    runTestParser item ""      ==? (unexpectedEOF "", "")

testDigitCombinator :: Spec
testDigitCombinator = describe "Digit combinator" $ do
  it "Consumes a single digit character from a digit populated stream" $ do
    runTestParser digit "123" ==> ('1', "23")
  it "Fails when the character is not a digit" $ do
    runTestParser digit "c"   ==? (expectedButGot "'c'" "digit '0..9'" "", "c")
