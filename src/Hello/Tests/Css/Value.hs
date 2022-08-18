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



-- TODO: restore the test. Or not. The function parsed here is being replaced by a bit different function.
{-
-- Tests for tokenAsValue* functions
--
-- These test cases don't specify parser's remainder before and after parsing.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenAsValueTestManualData1 = [

  -- Success
    AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["something", "other", "auto"]
                , tokenBefore1 = CssTokIdent "something"
                , expectedCssValue1 = Just (CssValueTypeEnum 0)
                }
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["zeroth", "first", "second", "third", "fourth", "fifth"]
                , tokenBefore1 = CssTokIdent "fifth"
                , expectedCssValue1 = Just (CssValueTypeEnum 5)
                }
  -- Atypical data: empty list of enums.
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = []
                , tokenBefore1 = CssTokIdent "auto"
                , expectedCssValue1 = Nothing
                }
  -- Atypical data: empty string in token.
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["sun", "stars", "moon"]
                , tokenBefore1 = CssTokIdent ""
                , expectedCssValue1 = Nothing
                }

  -- Failure: no such string on list of enums
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["red", "blue", "orange"]
                , tokenBefore1 = CssTokIdent "elephant"
                , expectedCssValue1 = Nothing
                }
  ]
-}




-- Tests for tokenAsValueColor function
--
-- These test cases specify parser's remainder before and after parsing.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenAsValueColorHashTestManualData = [

  -- Success
    AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokHash CssHashId "fb5"  -- fb5 interpreted as rgb should be expanded to rrggbb in form of ffbb55
                , enums1 = []
                , expectedCssValue1 = Just (CssValueTypeColor 0xffbb55)
                }
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokHash CssHashId "12de56"
                , enums1 = []
                , expectedCssValue1 = Just (CssValueTypeColor 0x12de56)
                }
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokIdent "red" -- Simple name
                , enums1 = []
                , expectedCssValue1 = Just (CssValueTypeColor 0xff0000)
                }
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokIdent "antiquewhite" -- "Sophisticated" name
                , enums1 = []
                , expectedCssValue1 = Just (CssValueTypeColor 0xfaebd7)
                }

  -- Failure, not a hex-digit string.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokHash CssHashId "ident" -- This is a valid hash token, but can't be converted to color.
                , enums1 = []
                , expectedCssValue1 = Nothing
                }
  -- Failure, incorrect count of hex characters.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , enums1 = []
                , tokenBefore1 = CssTokHash CssHashId "abcd" -- Color should have format rgb or rrggbb, so either 3 or 6 digits.
                , expectedCssValue1 = Nothing
                }
  -- Failure, empty hash string.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , enums1 = []
                , tokenBefore1 = CssTokHash CssHashId ""
                , expectedCssValue1 = Nothing
                }
  -- Failure, empty symbol string.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , enums1 = []
                , tokenBefore1 = CssTokIdent ""
                , expectedCssValue1 = Nothing
                }
  ]




tokenAsValueColorRgbTestManualData = [

  -- Success
    AsTestData2 { testedFunction2 = tokensAsValueColor
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokFunc "rgb"
                , remainderBefore2 = "15,50,200); next-property"
                , remainderAfter2  = " next-property"
                , tokenAfter2 = CssTokSemicolon
                , expectedCssValue2 = Just (CssValueTypeColor 0x0f32c8)
                }
  , AsTestData2 { testedFunction2 = tokensAsValueColor
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokFunc "rgb"
                , remainderBefore2 = "90%,20%,0%); next-property"
                , remainderAfter2  = " next-property"
                , tokenAfter2 = CssTokSemicolon
                , expectedCssValue2 = Just (CssValueTypeColor 0xe63300)
                }

  -- Percentage values over 100% or under 0% should be clipped.
  , AsTestData2 { testedFunction2 = tokensAsValueColor
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokFunc "rgb"
                , remainderBefore2 = "120%,-20%,15%); next-property" -- -> 100%,0%,15% -> 0xff0026
                , remainderAfter2  = " next-property"
                , tokenAfter2 = CssTokSemicolon
                , expectedCssValue2 = Just (CssValueTypeColor 0xff0026)
                }


  -- Mix of dimensionless values and percentages should be rejected.
  , AsTestData2 { testedFunction2 = tokensAsValueColor
                , enums2 = [] -- Doesn't matter for this tested function.
                , tokenBefore2 = CssTokFunc "rgb"
                , remainderBefore2 = "90%,20,0%); next-property"
                , remainderAfter2  = " next-property"
                , tokenAfter2 = CssTokSemicolon
                , expectedCssValue2 = Nothing
                }
      ]


-- TODO: restore the test
{-
-- Tests for tokenAsValueMultiEnum function
--
-- These test cases specify parser's remainder before and after parsing.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenAsValueMultiEnumTestManualData = [

  -- Success
      AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third"]
                  , tokenBefore2 = CssTokIdent "zeroth"
                  -- Declaration value ends with ';' here. Tokenizer will take
                  -- the char but tested function will keep it as current
                  -- token.
                  , remainderBefore2 = "first second third; next-property"
                  , remainderAfter2  = " next-property"
                  , tokenAfter2 = CssTokSemicolon
                  , expectedCssValue2 = Just (CssValueTypeMultiEnum 0b1111)
                  }
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third"]
                  , tokenBefore2 = CssTokIdent "zeroth"
                  -- Declaration value ends with '}' here. Tokenizer will take
                    -- the char but tested function will keep it as current
                  -- token.
                  , remainderBefore2 = "first third}selector"
                  , remainderAfter2  = "selector"
                  , tokenAfter2 = CssTokBraceCurlyClose
                  , expectedCssValue2 = Just (CssValueTypeMultiEnum 0b1011)
                  }
    -- Tokens in stream appear in order that does not match order of enums.
    -- Tested function should not care about the order.
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third", "fourth"]
                  , tokenBefore2 = CssTokIdent "second"
                  -- Declaration value ends with EOF..
                  , remainderBefore2 = "fourth zeroth"
                  , remainderAfter2  = ""
                  , tokenAfter2 = CssTokEnd
                  , expectedCssValue2 = Just (CssValueTypeMultiEnum 0b10101)
                  }


    -- Remainder tokens with values not on list of enums should lead to Nothing.
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third"]
                  , tokenBefore2 = CssTokIdent "third"
                  , remainderBefore2 = "gummi bears"
                  , remainderAfter2  = "gummi bears"
                  , tokenAfter2 = CssTokIdent "third"
                  , expectedCssValue2 = Nothing
                }
    -- Current token with value not on list of enums should also lead to Nothing.
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["water", "tea", "coffe", "pop"]
                  , tokenBefore2 = CssTokIdent "candy"
                  -- Since the current token won't be matched, we expect the function to return unmodified (p, t) pair.
                  , remainderBefore2 = "water tea coffe"
                  , remainderAfter2  = "water tea coffe"
                  , tokenAfter2 = CssTokIdent "candy"
                  , expectedCssValue2 = Nothing
                  }
    -- TODO: add test with input remainder containing something other than
    -- symbols/strings/identifiers, e.g. color or integer. This should fail
    -- for the same reason as above.
    ]
-}


-- Tests for tokenAsValueAuto function
--
-- These test cases specify parser's remainder before and after parsing.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenAsValueAutoTestManualData = [

  -- Success
      AsTestData2 { testedFunction2 = tokensAsValueAuto
                  , enums2 = [] -- Doesn't matter for this tested function.
                  , tokenBefore2 = CssTokIdent "auto"
                  , remainderBefore2 = "oxygen; next-property"
                  , remainderAfter2  = "; next-property"
                  , tokenAfter2 = CssTokIdent "oxygen"
                  , expectedCssValue2 = Just . CssValueTypeAuto $ CssDistanceAuto
                  }
    , AsTestData2 { testedFunction2 = tokensAsValueAuto
                  , enums2 = [] -- Doesn't matter for this tested function.
                  , tokenBefore2 = CssTokIdent "AUto"
                  , remainderBefore2 = "nitro}"
                  , remainderAfter2  = "}"
                  , tokenAfter2 = CssTokIdent "nitro"
                  , expectedCssValue2 = Just . CssValueTypeAuto $ CssDistanceAuto
                  }

    -- Failure. Current token is not "auto".
    , AsTestData2 { testedFunction2 = tokensAsValueAuto
                  , enums2 = [] -- Doesn't matter for this tested function.
                  , tokenBefore2 = CssTokIdent "gemini"
                  , remainderBefore2 = "taurus}"
                  , remainderAfter2  = "taurus}"
                  , tokenAfter2 = CssTokIdent "gemini"
                  , expectedCssValue2 = Nothing
                  }
      ]




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
      -- TestCase (do assertEqual "manual tests of tokenAsValue 1"            Nothing (tokenAsValueTest1 0 tokenAsValueTestManualData1))
     -- , TestCase (do assertEqual "manual tests of tokenAsValueMultiEnum"     Nothing (tokenAsValueTest2 0 tokenAsValueMultiEnumTestManualData))
     TestCase (do assertEqual "manual tests of tokenAsValueAuto"          Nothing (tokenAsValueTest2 0 tokenAsValueAutoTestManualData))
   , TestCase (do assertEqual "manual tests of tokenAsValueStringList"    Nothing (tokenAsValueTest2 0 tokenAsValueStringListTestManualData))
   , TestCase (do assertEqual "manual tests of tokenAsValueColor - hash"  Nothing (tokenAsValueTest1 0 tokenAsValueColorHashTestManualData))
   , TestCase (do assertEqual "manual tests of tokenAsValueColor - rgb"   Nothing (tokenAsValueTest2 0 tokenAsValueColorRgbTestManualData))
  ]




testsCssValue :: IO String
testsCssValue = do
  counts <- runTestTT (TestList (valueTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssValue failed"

