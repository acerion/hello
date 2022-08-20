{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Value
  (
    testsCssValue
  )
where




import qualified Data.Text as T
import Test.HUnit

import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils




-- Data type for functions that don't need CssValueType as first arg.
data AsTestData1 = AsTestData1 {
    testedFunction1   :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
  , enums1            :: [T.Text] -- Enumeration of texts to match tokens against.
  , tokenBefore1      :: CssToken -- Current token in (p, t) pair passed to tested function.
  , expectedCssValue1 :: Maybe CssValue
  }




-- Data type for functions that don't need CssValueType as first arg.
-- Data type with parser's remainder.
data AsTestData2 = AsTestData2 {
    testedFunction2   :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
  , enums2            :: [T.Text]  -- Enumeration of texts to match tokens against.
  , tokenBefore2      :: CssToken  -- Current token in (p, t) pair passed to tested function.
  , remainderBefore2  :: T.Text    -- 'remainder' field of parser in (p, t) pair passed to tested function.
  , remainderAfter2   :: T.Text    -- 'remainder' field of parser in (p, t) pair returned by tested function.
  , tokenAfter2       :: CssToken  -- Current token in (p, t) pair returned by tested function.
  , expectedCssValue2 :: Maybe CssValue
  }




-- Tests for tokenAsValueStringList function
--
-- These test cases specify parser's remainder before and after parsing.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenAsValueStringListTestManualData = [

  -- Success
    AsTestData2 { testedFunction2 = tokensAsValueStringList
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokIdent "monday"
                , remainderBefore2 = ",tuesday, wednesday , thursday; next-property"
                , remainderAfter2  = " next-property"
                , tokenAfter2 = CssTokSemicolon
                , expectedCssValue2 = Just (CssValueTypeStringList ["monday", "tuesday", "wednesday", "thursday"])
                }
  , AsTestData2 { testedFunction2 = tokensAsValueStringList
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokIdent "monday"
                , remainderBefore2 = "; next-property"
                , remainderAfter2  = " next-property"
                , tokenAfter2 = CssTokSemicolon
                , expectedCssValue2 = Just (CssValueTypeStringList ["monday"])
                }

  -- Hash token won't be interpreted as valid token for a list, and none of
  -- following tokens will.
  , AsTestData2 { testedFunction2 = tokensAsValueStringList
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokHash CssHashId "monday"
                , remainderBefore2 = "tuesday; next-property"
                , remainderAfter2  = "tuesday; next-property"
                , tokenAfter2 = CssTokHash CssHashId "monday"
                , expectedCssValue2 = Nothing
                }
  -- Numeric tokens should also lead to invalid parsing.
  , AsTestData2 { testedFunction2 = tokensAsValueStringList
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokIdent "monday"
                , remainderBefore2 = "tuesday, 99redbaloons, wednesday; next-property"
                , remainderAfter2  = "tuesday, 99redbaloons, wednesday; next-property"
                , tokenAfter2 = CssTokIdent "monday"
                , expectedCssValue2 = Nothing
                }
      ]




-- On success return Nothing. On failure return Just index of failed test.
tokenAsValueTest1 :: Int -> [AsTestData1] -> Maybe Int
tokenAsValueTest1 _ []       = Nothing
tokenAsValueTest1 idx (x:xs) = if (expectedCssValue1 x) /= value
                               then Just idx
                               else tokenAsValueTest1 (idx + 1) xs
  where
    ((_, _), value) = (testedFunction1 x) (defaultParser, tokenBefore1 x) (enums1 x)




-- On success return Nothing. On failure return Just index of failed test.
tokenAsValueTest2 :: Int -> [AsTestData2] -> Maybe Int
tokenAsValueTest2 _ []       = Nothing
tokenAsValueTest2 idx (x:xs) = if (expectedCssValue2 x) /= value || (remainderAfter2 x /= remainder p) || (tokenAfter2 x /= t)
                               then Just idx
                               else tokenAsValueTest2 (idx + 1) xs
  where
    ((p, t), value) = (testedFunction2 x) (defaultParser{remainder = remainderBefore2 x}, tokenBefore2 x) (enums2 x)




valueTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do assertEqual "manual tests of tokenAsValueStringList"    Nothing (tokenAsValueTest2 0 tokenAsValueStringListTestManualData))
  ]




testsCssValue :: IO String
testsCssValue = do
  counts <- runTestTT (TestList (valueTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssValue failed"

