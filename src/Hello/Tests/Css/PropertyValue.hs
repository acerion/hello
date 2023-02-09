{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}




module Hello.Tests.Css.PropertyValue
  (
    testsCssPropertyValue
  )
where




import qualified Data.Text as T

--import Debug.Trace

import Test.HUnit

import Hello.Colors
import Hello.Css.Distance
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Utils.Parser




-- --------------------------------------------------------------------------
-- Common definitions
-- --------------------------------------------------------------------------



-- propValueT: type of value of a CSS property, e.g. CssValueBackgroundColor
data TestData propValueT = TestData
  { inputPat       :: (CssParser, CssToken) -- initial parser+token
  , expectedResult :: Maybe ((CssParser, CssToken), propValueT)
  , testedFunction :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), propValueT)
  }




-- Test data for code testing parsers.
data TestDataP v = TestDataP
  { inputPatP       :: (CssParser, CssToken) -- initial parser+token
  , expectedResultP :: Maybe ((CssParser, CssToken), v)
  , testedFunctionP :: Parser (CssParser, CssToken) v
  }




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
--
testFunction :: (Show propValueT, Eq propValueT) => [TestData propValueT] -> T.Text
testFunction []     = ""
testFunction (x:xs) = if not (success result (expectedResult x))
                      then T.pack ("Got: " ++ show result ++ ", Expected: " ++ show (expectedResult x))
                      else testFunction xs
  where
    result = testedFunction x (inputPat x)

    -- See if test succeeded, i.e. if first argument (actual result of
    -- running a tested function) is equal to second argument (expected
    -- result of running a test).
    --
    -- When comparing parsers, this function compares only remainders because
    -- getting other fields of parsers right would require additional setup
    -- in test data.
    success :: (Show propValueT, Eq propValueT) => Maybe ((CssParser, CssToken), propValueT) -> Maybe ((CssParser, CssToken), propValueT) -> Bool
    -- This first case is a success because we expected Nothing and tested
    -- function returned Nothing, so everything went according to
    -- expecations.
    success Nothing Nothing = True
    success (Just (pat1, val1)) (Just (pat2, val2)) = (remainder . fst $ pat1) == (remainder . fst $ pat2) -- Parsers
                                                      && snd pat1 == snd pat2                              -- Tokens
                                                      && val1 == val2                                      -- Parsed values
    success _ _             = False




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
--
testFunctionP :: (Show propValueT, Eq propValueT) => [TestDataP propValueT] -> T.Text
testFunctionP []     = ""
testFunctionP (x:xs) = if not (success result (expectedResultP x))
                       then T.pack ("Got: " ++ show result ++ ", Expected: " ++ show (expectedResultP x))
                       else testFunctionP xs
  where
    result = runParser (testedFunctionP x) (inputPatP x)

    -- See if test succeeded, i.e. if first argument (actual result of
    -- running a tested function) is equal to second argument (expected
    -- result of running a test).
    --
    -- When comparing parsers, this function compares only remainders because
    -- getting other fields of parsers right would require additional setup
    -- in test data.
    success :: (Show propValueT, Eq propValueT) => Maybe ((CssParser, CssToken), propValueT) -> Maybe ((CssParser, CssToken), propValueT) -> Bool
    -- This first case is a success because we expected Nothing and tested
    -- function returned Nothing, so everything went according to
    -- expecations.
    success Nothing Nothing = True
    success (Just (pat1, val1)) (Just (pat2, val2)) = (remainder . fst $ pat1) == (remainder . fst $ pat2) -- Parsers
                                                      && snd pat1 == snd pat2                              -- Tokens
                                                      && val1 == val2                                      -- Parsed values
    success _ _             = False




-- --------------------------------------------------------------------------
-- Tests of mkParserEnum
-- --------------------------------------------------------------------------




-- An artifical set of values of some artifical CSS property.
data EnumTestData
  = EnumTestDataFirst
  | EnumTestDataSecond
  | EnumTestDataThird
  | EnumTestDataFourth
  | EnumTestDataFifth
  deriving (Enum, Eq, Show)




-- The test code will be testing if parsing a string token will result in
-- proper Haskell enum value. This dict is specifying the mapping from the
-- string to enum.
enumTestDict :: [(T.Text, EnumTestData)]
enumTestDict = [ ("first",    EnumTestDataFirst)
               , ("second",   EnumTestDataSecond)
               , ("third",    EnumTestDataThird)
               , ("fourth",   EnumTestDataFourth)
               , ("fifth",    EnumTestDataFifth)
               ]




-- TODO: add tests of strings with capital letters after you verify expected
-- behaviour in CSS spec.
mkParserEnumTestData :: [TestDataP EnumTestData]
mkParserEnumTestData =
  [
    -- Success case. First element on the list matches.
    TestDataP { testedFunctionP = mkParserEnum enumTestDict
              , inputPatP       =       (defaultParserInBlock "!important;", CssTokIdent "first")
              , expectedResultP = Just ((defaultParserInBlock "important;" , CssTokDelim '!'), EnumTestDataFirst)
              }

    -- Success case. Middle element on the list matches.
  , TestDataP { testedFunctionP = mkParserEnum enumTestDict
              , inputPatP       =       (defaultParserInBlock "!important;", CssTokIdent "third")
              , expectedResultP = Just ((defaultParserInBlock "important;",  CssTokDelim '!'), EnumTestDataThird)
              }

    -- Success case. Last element on the list matches.
  , TestDataP { testedFunctionP = mkParserEnum enumTestDict
              , inputPatP       =       (defaultParserInBlock "!important;", CssTokIdent "fifth")
              , expectedResultP = Just ((defaultParserInBlock "important;",  CssTokDelim '!'), EnumTestDataFifth)
              }

    -- Failure case: token not matching a dict. This may happen if input
    -- document supports newer CSS standard, and the implementation
    -- implements older CSS standard.
  , TestDataP { testedFunctionP = mkParserEnum enumTestDict
              , inputPatP       = (defaultParserInBlock "!important;", CssTokIdent "eight")
              , expectedResultP = Nothing
              }

    -- Failure case: empty dict. Not going to happen in practice because that
    -- would be a coding error that would be caught by other tests, and would
    -- require a deliberate omission of dictionary in parser code. Such
    -- situation will not be triggered by incoming CSS data. But still this
    -- is an interesting case worth testing.
  , TestDataP { testedFunctionP = mkParserEnum []
              , inputPatP       = (defaultParserInBlock "!important;", CssTokIdent "first")
              , expectedResultP = Nothing
              }

    -- Atypical data: empty string in token.
  , TestDataP { testedFunctionP = mkParserEnum enumTestDict
              , inputPatP       = (defaultParserInBlock "!important;", CssTokIdent "")
              , expectedResultP = Nothing
              }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsMultiEnum
-- --------------------------------------------------------------------------




-- An artifical set of values of some artifical CSS property.
data MultiEnumTestDataMultiEnum
  = MultiEnumTestDataMultiEnumFirst
  | MultiEnumTestDataMultiEnumSecond
  | MultiEnumTestDataMultiEnumThird
  | MultiEnumTestDataMultiEnumFourth
  | MultiEnumTestDataMultiEnumFifth
  deriving (Enum, Eq, Show)




-- The test code will be testing if parsing a string token will result in
-- proper Haskell enum value. This dict is specifying the mapping from the
-- string to enum.
multiEnumTestDict :: [(T.Text, MultiEnumTestDataMultiEnum)]
multiEnumTestDict = [ ("first",    MultiEnumTestDataMultiEnumFirst)
                    , ("second",   MultiEnumTestDataMultiEnumSecond)
                    , ("third",    MultiEnumTestDataMultiEnumThird)
                    , ("fourth",   MultiEnumTestDataMultiEnumFourth)
                    , ("fifth",    MultiEnumTestDataMultiEnumFifth)
                    ]




-- TODO: add tests of strings with capital letters after you verify expected
-- behaviour in CSS spec.
--
-- TODO: add test where a valid token (e.g. "second") appears more than once
-- in the input.
multiEnumTestData :: [TestData [MultiEnumTestDataMultiEnum]]
multiEnumTestData =
  [
    -- Success case. All elements on the list are found in CSS input.
    --
    -- Declaration value ends with ';' here. Tokenizer will take the char but
    -- tested function will keep it as current token.
    TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       =       (defaultParserInBlock "second third fourth fifth !important;", CssTokIdent "first")
             , expectedResult = Just ((defaultParserInBlock "important;"                           , CssTokDelim '!')
                                     , [ MultiEnumTestDataMultiEnumFirst
                                       , MultiEnumTestDataMultiEnumSecond
                                       , MultiEnumTestDataMultiEnumThird
                                       , MultiEnumTestDataMultiEnumFourth
                                       , MultiEnumTestDataMultiEnumFifth
                                       ]
                                     )
             }

    -- Success case.
    --
    -- Declaration value ends with '}' here. Tokenizer will take the char but
    -- tested function will keep it as current token.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       =       (defaultParserInBlock "third fourth} selector", CssTokIdent "second")
             , expectedResult = Just ((defaultParserInBlock " selector"             , CssTokBraceCurlyClose)
                                     , [ MultiEnumTestDataMultiEnumSecond
                                       , MultiEnumTestDataMultiEnumThird
                                       , MultiEnumTestDataMultiEnumFourth
                                       ]
                                     )
             }

    -- Success case.
    --
    -- Tokens in input text appear in order that does not match order of
    -- enums. Tested function should not care about the order.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       =       (defaultParserInBlock "fifth first; selector", CssTokIdent "second")
             , expectedResult = Just ((defaultParserInBlock " selector"            , CssTokSemicolon)
                                     , [ MultiEnumTestDataMultiEnumSecond
                                       , MultiEnumTestDataMultiEnumFifth
                                       , MultiEnumTestDataMultiEnumFirst
                                       ]
                                     )
             }


    -- Failure case.
    --
    -- Remainder tokens with values not on list of enums should result in Nothing.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "hello there first; selector", CssTokIdent "second")
             , expectedResult = Nothing
             }

    -- Failure case.
    --
    -- Current token with value not on list of enums should also result in Nothing.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "second third fourth; selector", CssTokIdent "red")
             , expectedResult = Nothing
             }

    -- Failure case.
    --
    -- One of remainder tokens is a color, so the result is Nothing.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "hello #aabbcc first; selector", CssTokIdent "second")
             , expectedResult = Nothing
             }

    -- Failure case.
    --
    -- Current token is a color, so the result is Nothing.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "second third fourth; selector", CssTokIdent "#aabbcc")
             , expectedResult = Nothing
             }

{-  -- FIXME: the tested function recognizes current token as valid token, but
    -- then it doesn't discard "12px" in remainder as something unexpected.
    -- The result of the test function is "Just
    -- [MultiEnumTestDataMultiEnumSecond]", while it probably should be
    -- Nothing.

    -- Failure case.
    --
    -- One of remainder tokens is a length, so the result is Nothing.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "12px !important", CssTokIdent "second")
             , expectedResult = Nothing
             }
-}

    -- Failure case.
    --
    -- Current token is a length, so the result is Nothing.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "second third fourth; selector", CssTokPerc . CssNumF $ 50.0)
             , expectedResult = Nothing
             }


    -- Failure case: empty dict. Not going to happen in practice because that
    -- would be a coding error that would be caught by other tests, and would
    -- require a deliberate omission of dictionary in parser code. Such
    -- situation will not be triggered by incoming CSS data. But still this
    -- is an interesting case worth testing.
  , TestData { testedFunction = interpretTokensAsMultiEnum []
             , inputPat       = (defaultParserInBlock "first third; selector", CssTokIdent "second")
             , expectedResult = Nothing
             }


    -- Atypical data: empty string in token.
  , TestData { testedFunction = interpretTokensAsMultiEnum multiEnumTestDict
             , inputPat       = (defaultParserInBlock "second third fourth fifth !important;", CssTokIdent "")
             , expectedResult = Nothing
             }
  ]




-- --------------------------------------------------------------------------
-- Tests of parserDistanceAuto
-- --------------------------------------------------------------------------




-- An artifical value ctor for value of some artifical CSS property.
data AutoTestValue = AutoTestValueCtor CssDistance
  deriving (Eq, Show)




-- TODO: add tests of strings with capital letters after you verify expected
-- behaviour in CSS spec.
--
-- TODO: notice that the actual value of property that is being parsed in the
-- first example is "auto something;". This doesn't look like a string that
-- should be successfully parsed as "auto". Should we reject an "auto
-- something" string as something that can't be parsed as "auto"?
parserDistanceAutoTestData :: [TestDataP CssDistance]
parserDistanceAutoTestData =
  [
    -- Success case. Current token is "auto".
    --
    -- Declaration value ends with ';' here. Tokenizer will take the char but
    -- tested function will keep it as current token.
    TestDataP { testedFunctionP = parserDistanceAuto
              , inputPatP       =       (defaultParserInBlock "something; other", CssTokIdent "auto")
              , expectedResultP = Just ((defaultParserInBlock "; other",          CssTokIdent "something"), CssDistanceAuto)
              }

    -- Failure case. Current token is not "auto".
  , TestDataP { testedFunctionP = parserDistanceAuto
              , inputPatP       = (defaultParserInBlock "something; other", CssTokIdent "italic")
              , expectedResultP = Nothing
              }

    -- Failure case. Current token is definitely not "auto".
  , TestDataP { testedFunctionP = parserDistanceAuto
              , inputPatP       = (defaultParserInBlock "something; other", CssTokBraceCurlyClose)
              , expectedResultP = Nothing
              }

    -- Failure case. Current token is definitely not "auto".
  , TestDataP { testedFunctionP = parserDistanceAuto
              , inputPatP       = (defaultParserInBlock "something; other", CssTokDelim '@')
              , expectedResultP = Nothing
              }
  ]




-- --------------------------------------------------------------------------
-- Tests of parserColor
-- --------------------------------------------------------------------------




colorTestData1 :: [TestDataP Color]
colorTestData1 =
  [
    -- Success case. Color as hex.
    TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other1", CssTokHash CssHashId "fb5")  -- fb5 interpreted as rgb should be expanded to rrggbb in form of ffbb55
              , expectedResultP = Just ((defaultParserInBlock "; other1",          CssTokIdent "something"), 0xffbb55)
              }

    -- Success case. Color as hex.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other2", CssTokHash CssHashId "12de56")
              , expectedResultP = Just ((defaultParserInBlock "; other2",         CssTokIdent "something"), 0x12de56)
              }

    -- Success case. Color as hex. Capital letters in hex string.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other3", CssTokHash CssHashId "12DE5A")
              , expectedResultP = Just ((defaultParserInBlock "; other3",          CssTokIdent "something"), 0x12de5A)
              }

    -- Success case. Color as name.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other4", CssTokIdent "red")
              , expectedResultP = Just ((defaultParserInBlock "; other4",          CssTokIdent "something"), 0xff0000)
              }

    -- Success case. Color as less frequently used name.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other5", CssTokIdent "antiquewhite")
              , expectedResultP = Just ((defaultParserInBlock "; other5",          CssTokIdent "something"), 0xfaebd7)
              }

    -- Failure case. Name is not a proper color name.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other6", CssTokIdent "czerwony")
              , expectedResultP = Nothing
              }

    -- Failure, hash is not a hex-digit string.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other7", CssTokHash CssHashId "ident")
              , expectedResultP = Nothing
              }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other8", CssTokHash CssHashId "a")
              , expectedResultP = Nothing
              }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other9", CssTokHash CssHashId "ab")
              , expectedResultP = Nothing
              }

    -- Success, just a sanity check.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other10", CssTokHash CssHashId "abc")
              , expectedResultP = Just ((defaultParserInBlock "; other10",          CssTokIdent "something"), 0xaabbcc)
              }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other11", CssTokHash CssHashId "abcd")
              , expectedResultP = Nothing
              }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other12", CssTokHash CssHashId "abcde")
              , expectedResultP = Nothing
              }

    -- Success, just a sanity check.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "something; other13", CssTokHash CssHashId "abcdef")
              , expectedResultP = Just ((defaultParserInBlock "; other13",          CssTokIdent "something"), 0xabcdef)
              }

    -- Failure, empty hash string.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other14", CssTokHash CssHashId "")
              , expectedResultP = Nothing
              }

    -- Failure. Empty ident string.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other15", CssTokIdent "")
              , expectedResultP = Nothing
              }

    -- Failure. Unexpected current token type.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other16", CssTokDelim '@')
              , expectedResultP = Nothing
              }

  -- Failure. Unexpected current token type.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "something; other17", CssTokPerc . CssNumF $ 50.0)
              , expectedResultP = Nothing
              }
  ]




-- Input data is a rgb function
colorTestData2 :: [TestDataP Color]
colorTestData2 =
  [
    -- Success case. Color as rgb function.
    TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "15,50,200); next-property1",        CssTokFunc "rgb")
              , expectedResultP = Just ((defaultParserInBlock " next-property1", CssTokSemicolon), 0x0f32c8)
              }

    -- Success case. Color as rgb function, with percentages.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "90%,20%,0%); next-property2",       CssTokFunc "rgb")
              , expectedResultP = Just ((defaultParserInBlock " next-property2", CssTokSemicolon), 0xe63300)
              }

    -- Success case. Color as rgb function, with percentages.
    --
    -- Percentage values over 100% or under 0% should be clipped (in this
    -- case to 100%,0%,15%).
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       =       (defaultParserInBlock "120%,-20%,15%); next-property3", CssTokFunc "rgb")
              , expectedResultP = Just ((defaultParserInBlock " next-property3",                CssTokSemicolon), 0xff0026)
              }

    -- Failure. Mix of dimensionless values and percentages should be rejected.
  , TestDataP { testedFunctionP = parserColor
              , inputPatP       = (defaultParserInBlock "90%,20,0%); next-property4", CssTokFunc "rgb")
              , expectedResultP = Nothing
              }
  ]




-- --------------------------------------------------------------------------
-- Tests of mkParserRangeInteger
-- --------------------------------------------------------------------------




rangeIntegerTestData :: [TestDataP Int]
rangeIntegerTestData =
  [
    -- Success case. Numeric token with value within range.
    TestDataP { testedFunctionP = mkParserRangeInteger (0, 900)
              , inputPatP       =       (defaultParserInBlock"!important", CssTokNum . CssNumI $ 9)
              , expectedResultP = Just ((defaultParserInBlock "important", CssTokDelim '!'), 9)
              }

    -- Success case. Numeric token with value within range.
  , TestDataP { testedFunctionP = mkParserRangeInteger (-200, 900)
              , inputPatP       =       (defaultParserInBlock "!important", CssTokNum . CssNumI $ -150)
              , expectedResultP = Just ((defaultParserInBlock "important", CssTokDelim '!'), (-150))
              }

    -- Success case. Numeric token with value that is equal to lower bound of
    -- range.
  , TestDataP { testedFunctionP = mkParserRangeInteger (0, 900)
              , inputPatP       =       (defaultParserInBlock "!important", CssTokNum . CssNumI $ 0)
              , expectedResultP = Just ((defaultParserInBlock "important",  CssTokDelim '!'), 0)
              }

    -- Success case. Numeric token with value that is equal to upper bound of
    -- range.
  , TestDataP { testedFunctionP = mkParserRangeInteger (0, 900)
              , inputPatP       =       (defaultParserInBlock "!important", CssTokNum . CssNumI $ 900)
              , expectedResultP = Just ((defaultParserInBlock "important",  CssTokDelim '!'), 900)
              }

    -- Failure case. Numeric token with value that is below lower bound of
    -- range.
  , TestDataP { testedFunctionP = mkParserRangeInteger (100, 900)
              , inputPatP       = (defaultParserInBlock "!important", CssTokNum . CssNumI $ 99)
              , expectedResultP = Nothing
              }

    -- Failure case. Numeric token with value that is above lower bound of
    -- range.
  , TestDataP { testedFunctionP = mkParserRangeInteger (0, 900)
              , inputPatP       = (defaultParserInBlock "!important", CssTokNum . CssNumI $ 901)
              , expectedResultP = Nothing
              }
  ]




{- -------------------------------------------------------------------------- -}




-- If some error is found, test function returns non-empty string which can
-- help identify a test that failed.
testCases :: [Test]
testCases =
  [
    TestCase (do assertEqual "manual tests of mkParserEnum"                       "" (testFunctionP mkParserEnumTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsMultiEnum"         "" (testFunction multiEnumTestData))
  , TestCase (do assertEqual "manual tests of parserDistanceAuto"                 "" (testFunctionP parserDistanceAutoTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsColor (value)"     "" (testFunctionP colorTestData1))
  , TestCase (do assertEqual "manual tests of interpretTokensAsColor (rgb)"       "" (testFunctionP colorTestData2))
  , TestCase (do assertEqual "manual tests of mkParserRangeInteger"               "" (testFunctionP rangeIntegerTestData))
  ]




testsCssPropertyValue :: IO String
testsCssPropertyValue = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.PropertyValue failed"

