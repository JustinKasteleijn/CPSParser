module CombinatorsSpec (spec) where

import           Combinators
import           CustomTestLib
import           Data.Word     (Word8)
import           Parser        (Parser)
import           ParserError
import           Test.Syd

spec :: Spec
spec = do
  describe "Test for parser combinators" $ do
    testItemCombinator
    testDigitCombinator
    testSatisfyCombinator
    testNewlineCombinator
    testTabCombinator
    testAlphaCombinator
    testSepByCombinators
    testCharCombinator
    testStringCombinator
    testBetweenCombinator
    testWhitespaceCombinator
    testNatCombinator
    testIntCombinator
    testBoundedCombinator

testItemCombinator :: Spec
testItemCombinator = describe "Item combinator" $ do
  it "Consumes a single character from a populated stream" $ do
    runTestParser item "hello" ==> ('h', "ello")
  it "Fails gracefully with an EOF error when given an empty stream" $ do
    runTestParser item ""      ==? (unexpectedEOF "", "")

testSatisfyCombinator :: Spec
testSatisfyCombinator = describe "Satisfy combinator" $ do
  it "Consumes a single character from a polulated input where the predicate succeeds" $ do
    runTestParser (satisfy (=='h') "" show) "hello"    ==> ('h', "ello")
  it "Fails with expected but got error when given failing predicate" $ do
    runTestParser (satisfy (=='e') "e" show) "hello"   ==? (expectedButGot "'h'" "e" "", "hello")

testDigitCombinator :: Spec
testDigitCombinator = describe "Digit combinator" $ do
  it "Consumes a single digit character from a digit populated stream" $ do
    runTestParser digit "123" ==> ('1', "23")
  it "Fails when the character is not a digit" $ do
    runTestParser digit "c"   ==? (expectedButGot "'c'" "digit '0..9'" "", "c")

testNewlineCombinator :: Spec
testNewlineCombinator = describe "Newline combinator" $ do
  it "Conusmes a single newline character from a populated input" $ do
    runTestParser newline "\n" ==> ('\n', "")
  it "Fails when the character is not a newline" $ do
    runTestParser newline "c" ==? (expectedButGot "'c'" "\n" "", "c")

testTabCombinator :: Spec
testTabCombinator = describe "Tab combinator" $ do
  it "Consumes a single character from a populated input" $ do
    runTestParser tab "\t" ==> ('\t', "")
  it "Fails when the character is not a tab" $ do
    runTestParser tab "c" ==? (expectedButGot "'c'" "\t" "", "c")

testAlphaCombinator :: Spec
testAlphaCombinator = describe "Alpha combinator" $ do
  it "Consumes a single character from a lower case character populated input" $ do
    runTestParser alpha "hello" ==> ('h', "ello")
  it "Consumes a single character from a upper case character populated input" $ do
    runTestParser alpha "Hello" ==> ('H', "ello")
  it "Fails when the character is not a alpha" $ do
    runTestParser alpha "1Hello" ==? (expectedButGot "'1'" "alpha 'a..z|A..Z'" "", "1Hello")

testSepByCombinators :: Spec
testSepByCombinators = describe "Sepby combinators" $ do
  it "sepBy1 consumes a single comma separated character" $ do
    runTestParser (sepBy1 (char ',') item) "h,h"           ==> ("hh", "")
  it "sepBy1 leaves trailing separator" $ do
    runTestParser (sepBy1 (char ',') item) "h,h,"          ==> ("hh", ",")
  it "SepBy1 fails on empty input" $ do
    runTestParser (sepBy1 (char ',') item) ""              ==? (unexpectedEOF "", "")
  it "SepBy0 succeeds on empty input" $ do
    runTestParser (sepBy0 (char ',') item) ""              ==> ([], "")
  it "SepBy1Trailing succeeds and consumes on trailing separator" $ do
    runTestParser (sepBy1Trailing (char ',') item) "h,h,"  ==> ("hh", "")
  it "SepBy1Trailing succeeds without trailing separator" $ do
      runTestParser (sepBy1Trailing (char ',') item) "h,h" ==> ("hh", "")
  it "SepBy1Trailing fails on empty input" $ do
      runTestParser (sepBy1Trailing (char ',') item) ""    ==? (unexpectedEOF "", "")
  it "SepBy0Trailing fails on empty input" $ do
      runTestParser (sepBy0Trailing (char ',') item) ""    ==> ("", "")

testCharCombinator :: Spec
testCharCombinator = describe "Char combinator" $ do
  it "Consumes a single char on populated input" $ do
    runTestParser (char 'p') "pp" ==> ('p', "p")

testStringCombinator :: Spec
testStringCombinator = describe "String combinator" $ do
  it "consumes a string when string is given" $ do
    runTestParser (string "hello") "hello" ==> ("hello", "")

testBetweenCombinator :: Spec
testBetweenCombinator = describe "Between combinator" $ do
  it "Consumes middle when boundaries are specified" $ do
    runTestParser (between (char '(') (char ')') item) "(a)" ==> ('a', "")
  it "Parser fails when left boundary is not defined" $ do
    runTestParser (between (char '(') (char ')') item) "a)"  ==? (expectedButGot "'a'" "(" "", "a)")

testWhitespaceCombinator :: Spec
testWhitespaceCombinator = describe "Whitespace combinator" $ do
  it "Consumes a single whitespace on populated input" $ do
     runTestParser whitespace " " ==> ((), "")
  it "Fails on non whitespace character" $ do
    runTestParser whitespace "c"  ==? (expectedButGot "'c'" "whitespace" "", "c")

testNatCombinator :: Spec
testNatCombinator = describe "Nat combinatot" $ do
  it "Consumes a single number and returns it as integer" $ do
    runTestParser (unsignedInt :: Parser String String Int) "1" ==> (1, "")
  it "Consumes multiple numbers as a single integer" $ do
    runTestParser (unsignedInt :: Parser String String Int) "123" ==> (123, "")
  it "Fails when a negative integer is given" $ do
    runTestParser (unsignedInt :: Parser String String Int) "-12" ==? (expectedButGot "'-'" "digit '0..9'" "", "-12")

testIntCombinator :: Spec
testIntCombinator = describe "Int combinator" $ do
  it "Consumes negative numbers as negative integers" $ do
    runTestParser (signedInt :: Parser String String Int) "-123" ==> (-123, "")

testBoundedCombinator :: Spec
testBoundedCombinator = describe "Int combinator" $ do
  it "Consumes value between valid min and max bound (u8)" $ do
    runTestParser (parseBounded :: Parser String String Word8) "234" ==> (234, "")
  it "Consumes the max bound (u8)" $ do
    runTestParser (parseBounded :: Parser String String Word8) "255" ==> (255, "")
  it "Consumes the min bound (u8)" $ do
    runTestParser (parseBounded :: Parser String String Word8) "0"   ==> (0, "")
  it "Rejects value outside of min and max bound (u8)" $ do
    runTestParser (parseBounded :: Parser String String Word8) "257" ==? (expectedButGot "257" "a value between 0 and 255" "", "")
