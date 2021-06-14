{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module TestsCssParser (testsCssTokenizer
                      ) where




import qualified Data.Text as T
import Test.HUnit
import System.Exit
import CssParser
import HelloUtils




-- Tests for parsing strings as <number-token> (CssTokNumX),
-- <percentage-token> (CssTokPercX) or <dimension-token> (CssTokDimX).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenizerNumbersTestManualData = [
  -- parser's remainder before     expected token           parser's remainder after

  -- Tests of <number-token>
    ( "0",                     CssTokNum $ CssNumI 0,                ""  )
  , ( "0}",                    CssTokNum $ CssNumI 0,                "}" )
  , ( "-0}",                   CssTokNum $ CssNumI 0,                "}" )
  , ( "10}",                   CssTokNum $ CssNumI 10,               "}" )
  , ( "+10}",                  CssTokNum $ CssNumI 10,               "}" )
  , ( "-10}",                  CssTokNum $ CssNumI (-10),            "}" )
  , ( "56298453554,",          CssTokNum $ CssNumI 56298453554,      "," )
  , ( "512 dragons",           CssTokNum $ CssNumI 512,              " dragons" )
  , ( "+512 dragons",          CssTokNum $ CssNumI 512,              " dragons" )
  , ( "-512 dragons",          CssTokNum $ CssNumI (-512),           " dragons" )

  , ( "0.0",                   CssTokNum $ CssNumF 0.0,              ""  )
  , ( "0.0}",                  CssTokNum $ CssNumF 0.0,              "}" )
  , ( "+0.0}",                 CssTokNum $ CssNumF 0.0,              "}" )
  , ( "-0.0}",                 CssTokNum $ CssNumF (-0.0),           "}" )
  , ( "0.1}",                  CssTokNum $ CssNumF 0.1,              "}" )
  , ( "+0.1}",                 CssTokNum $ CssNumF 0.1,              "}" )
  , ( "-0.1}",                 CssTokNum $ CssNumF (-0.1),           "}" )
  , ( "12.333}",               CssTokNum $ CssNumF 12.333,           "}" )
  , ( "+12.333}",              CssTokNum $ CssNumF 12.333,           "}" )
  , ( "-12.333}",              CssTokNum $ CssNumF (-12.333),        "}" )
  , ( "12345.1,",              CssTokNum $ CssNumF 12345.1,          "," )
  , ( "76.5 computers",        CssTokNum $ CssNumF 76.5,             " computers" )
  , ( "44.2,",                 CssTokNum $ CssNumF 44.2,             "," )
  -- , ( ".2,",                   CssTokNum $ CssNumF 0.2,              "," ) -- TODO: implement support for this format
  -- , ( "10e2}",                 CssTokNum $ CssNumF 1000.0,           "}" ) -- TODO: implement support for this format
  , ( "1.2e-3}",               CssTokNum $ CssNumF 0.0012,           "}" )
  , ( "+1.2e-3}",              CssTokNum $ CssNumF 0.0012,           "}" )
  , ( "-1.2e-3}",              CssTokNum $ CssNumF (-0.0012),        "}" )




  -- Tests of <percentage-token>
  , ( "13%;",                  CssTokPerc $ CssNumI 13,              ";" )
  , ( "-13%;",                 CssTokPerc $ CssNumI (-13),           ";" )
  , ( "+13%;",                 CssTokPerc $ CssNumI 13,              ";" )
  -- This is a percentage followed immediately by some identifier
  , ( "31%px",                 CssTokPerc $ CssNumI 31,              "px" )

  , ( "99.9%;",                CssTokPerc $ CssNumF 99.9,            ";" )
  , ( "-99.9%;",               CssTokPerc $ CssNumF (-99.9),         ";" )
  , ( "+99.9%;",               CssTokPerc $ CssNumF 99.9,            ";" )
  -- This is a percentage followed immediately by some identifier
  , ( "45.8%px",               CssTokPerc $ CssNumF 45.8,            "px" )




  -- Tests of <dimension-token>
  , ( "127px;",                CssTokDim (CssNumI 127) "px",         ";" )
  , ( "-1pt ",                 CssTokDim (CssNumI (-1)) "pt",        " " )
  , ( "+1pt ",                 CssTokDim (CssNumI 1) "pt",           " " )
  -- Tokenizer doesn't validate correctness of units, it just appends
  -- identifier to number to get <dimension-token>. Verifying units is done
  -- by parser, not tokenizer.
  , ( "15cars}",               CssTokDim (CssNumI 15) "cars",        "}" )

  , ( "99.9mm;",               CssTokDim (CssNumF 99.9) "mm",        ";" )
  , ( "-15.5in ",              CssTokDim (CssNumF (-15.5)) "in",     " " )
  , ( "+15.5in ",              CssTokDim (CssNumF 15.5) "in",        " " )
  -- Tokenizer doesn't validate correctness of units, it just appends
  -- identifier to number to get <dimension-token>. Verifying units is done
  -- by parser, not tokenizer.
  , ( "64.22cars}",            CssTokDim (CssNumF 64.22) "cars",     "}" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
tokenizerNumbersTest :: [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerNumbersTest []     = ""
tokenizerNumbersTest (x:xs) = if expectedToken /= token || remainderAfter /= (remainder parser)
                              then remainderBefore
                              else tokenizerNumbersTest xs
  where
    remainderBefore = tripletFst x
    expectedToken   = tripletSnd x
    remainderAfter  = tripletThrd x
    (parser, token) = nextToken defaultParser{remainder = remainderBefore}





-- Tests for parsing strings as <hash-token> (CssTokHash).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenizerHashTestManualData = [
  -- parser's remainder before     expected token           parser's remainder after

    ( "#0",                    CssTokHash "0",                        ""  )
  , ( "#02553}",               CssTokHash "02553",                    "}" )
  , ( "#name+",                CssTokHash "name",                     "+" )
  , ( "#aD-9_1%",              CssTokHash "aD-9_1",                   "%" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
tokenizerHashTest :: [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerHashTest []     = ""
tokenizerHashTest (x:xs) = if expectedToken /= token || remainderAfter /= (remainder parser)
                           then remainderBefore
                           else tokenizerHashTest xs
  where
    remainderBefore = tripletFst x
    expectedToken   = tripletSnd x
    remainderAfter  = tripletThrd x
    (parser, token) = nextToken defaultParser{remainder = remainderBefore, inBlock = True}




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
                , tokenBefore1 = CssTokSym "something"
                , expectedCssValue1 = Just defaultValue{typeTag = CssValueTypeEnum, intVal = 0}
                }
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["zeroth", "first", "second", "third", "fourth", "fifth"]
                , tokenBefore1 = CssTokSym "fifth"
                , expectedCssValue1 = Just defaultValue{typeTag = CssValueTypeEnum, intVal = 5}
                }
  -- Atypical data: empty list of enums.
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = []
                , tokenBefore1 = CssTokSym "auto"
                , expectedCssValue1 = Nothing
                }
  -- Atypical data: empty string in token.
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["sun", "stars", "moon"]
                , tokenBefore1 = CssTokSym ""
                , expectedCssValue1 = Nothing
                }

  -- Failure: no such string on list of enums
  , AsTestData1 { testedFunction1 = tokensAsValueEnum
                , enums1 = ["red", "blue", "orange"]
                , tokenBefore1 = CssTokSym "elephant"
                , expectedCssValue1 = Nothing
                }



  -- Success
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokHash "fb5"  -- fb5 interpreted as rgb should be expanded to rrggbb in form of ffbb55
                , enums1 = []
                , expectedCssValue1 = Just defaultValue{typeTag = CssValueTypeColor, intVal = 0xffbb55}
                }
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokHash "12de56"
                , enums1 = []
                , expectedCssValue1 = Just defaultValue{typeTag = CssValueTypeColor, intVal = 0x12de56}
                }
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokSym "red" -- Simple name
                , enums1 = []
                , expectedCssValue1 = Just defaultValue{typeTag = CssValueTypeColor, intVal = 0xff0000}
                }
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokSym "antiquewhite" -- "Sophisticated" name
                , enums1 = []
                , expectedCssValue1 = Just defaultValue{typeTag = CssValueTypeColor, intVal = 0xfaebd7}
                }

  -- Failure, not a hex-digit string.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , tokenBefore1 = CssTokHash "ident" -- This is a valid hash token, but can't be converted to color.
                , enums1 = []
                , expectedCssValue1 = Nothing
                }
  -- Failure, incorrect count of hex characters.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , enums1 = []
                , tokenBefore1 = CssTokHash "abcd" -- Color should have format rgb or rrggbb, so either 3 or 6 digits.
                , expectedCssValue1 = Nothing
                }
  -- Failure, empty hash string.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , enums1 = []
                , tokenBefore1 = CssTokHash ""
                , expectedCssValue1 = Nothing
                }
  -- Failure, empty symbol string.
  , AsTestData1 { testedFunction1 = tokensAsValueColor
                , enums1 = []
                , tokenBefore1 = CssTokSym ""
                , expectedCssValue1 = Nothing
                }
  ]




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
                  , tokenBefore2 = CssTokSym "zeroth"
                  -- Declaration value ends with ';' here. Tokenizer will take
                  -- the char but tested function will keep it as current
                  -- token.
                  , remainderBefore2 = "first second third; next-property"
                  , remainderAfter2  = " next-property"
                  , tokenAfter2 = CssTokCh ';'
                  , expectedCssValue2 = Just defaultValue{typeTag = CssValueTypeMultiEnum, intVal = 0b1111}
                  }
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third"]
                  , tokenBefore2 = CssTokSym "zeroth"
                  -- Declaration value ends with '}' here. Tokenizer will take
                    -- the char but tested function will keep it as current
                  -- token.
                  , remainderBefore2 = "first third}selector"
                  , remainderAfter2  = "selector"
                  , tokenAfter2 = CssTokCh '}'
                  , expectedCssValue2 = Just defaultValue{typeTag = CssValueTypeMultiEnum, intVal = 0b1011}
                  }
    -- Tokens in stream appear in order that does not match order of enums.
    -- Tested function should not care about the order.
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third", "fourth"]
                  , tokenBefore2 = CssTokSym "second"
                  -- Declaration value ends with EOF..
                  , remainderBefore2 = "fourth zeroth"
                  , remainderAfter2  = ""
                  , tokenAfter2 = CssTokEnd
                  , expectedCssValue2 = Just defaultValue{typeTag = CssValueTypeMultiEnum, intVal = 0b10101}
                  }


    -- Remainder tokens with values not on list of enums should lead to Nothing.
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["zeroth", "first", "second", "third"]
                  , tokenBefore2 = CssTokSym "third"
                  , remainderBefore2 = "gummi bears"
                  , remainderAfter2  = "gummi bears"
                  , tokenAfter2 = CssTokSym "third"
                  , expectedCssValue2 = Nothing
                }
    -- Current token with value not on list of enums should also lead to Nothing.
    , AsTestData2 { testedFunction2 = tokensAsValueMultiEnum
                  , enums2 = ["water", "tea", "coffe", "pop"]
                  , tokenBefore2 = CssTokSym "candy"
                  -- Since the current token won't be matched, we expect the function to return unmodified (p, t) pair.
                  , remainderBefore2 = "water tea coffe"
                  , remainderAfter2  = "water tea coffe"
                  , tokenAfter2 = CssTokSym "candy"
                  , expectedCssValue2 = Nothing
                  }
    -- TODO: add test with input remainder containing something other than
    -- symbols/strings/identifiers, e.g. color or integer. This should fail
    -- for the same reason as above.
    ]


-- Tests for tokenAsValueMultiEnum function
--
-- These test cases specify parser's remainder before and after parsing.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenAsValueAutoTestManualData = [

  -- Success
      AsTestData2 { testedFunction2 = tokensAsValueAuto
                  , enums2 = [] -- Doesn't matter for this tested function.
                  , tokenBefore2 = CssTokSym "auto"
                  , remainderBefore2 = "oxygen; next-property"
                  , remainderAfter2  = "; next-property"
                  , tokenAfter2 = CssTokSym "oxygen"
                  , expectedCssValue2 = Just defaultValue{typeTag = CssValueTypeAuto, intVal = cssLengthTypeAuto}
                  }
    , AsTestData2 { testedFunction2 = tokensAsValueAuto
                  , enums2 = [] -- Doesn't matter for this tested function.
                  , tokenBefore2 = CssTokSym "AUto"
                  , remainderBefore2 = "nitro}"
                  , remainderAfter2  = "}"
                  , tokenAfter2 = CssTokSym "nitro"
                  , expectedCssValue2 = Just defaultValue{typeTag = CssValueTypeAuto, intVal = cssLengthTypeAuto}
                  }

    -- Failure. Current token is not "auto".
    , AsTestData2 { testedFunction2 = tokensAsValueAuto
                  , enums2 = [] -- Doesn't matter for this tested function.
                  , tokenBefore2 = CssTokSym "gemini"
                  , remainderBefore2 = "taurus}"
                  , remainderAfter2  = "taurus}"
                  , tokenAfter2 = CssTokSym "gemini"
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




tokenizerTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of numbers" "" (tokenizerNumbersTest tokenizerNumbersTestManualData))

   , TestCase (do
                 assertEqual "manual tests of hash"  "" (tokenizerHashTest tokenizerHashTestManualData))

   , TestCase (do
                 assertEqual "manual tests of tokenAsValue 1"  Nothing (tokenAsValueTest1 0 tokenAsValueTestManualData1))

   , TestCase (do
                 assertEqual "manual tests of tokenAsValueMultiEnum"  Nothing (tokenAsValueTest2 0 tokenAsValueMultiEnumTestManualData))

   , TestCase (do
                 assertEqual "manual tests of tokenAsValueAuto"  Nothing (tokenAsValueTest2 0 tokenAsValueAutoTestManualData))
  ]




testsCssTokenizer :: IO ()
testsCssTokenizer = do
  counts <- runTestTT (TestList (tokenizerTestCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure

