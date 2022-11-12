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

import Hello.Css.Distance
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer




-- --------------------------------------------------------------------------
-- Common definitions
-- --------------------------------------------------------------------------




data TestData propValueT = TestData
  { initialRem      :: T.Text                    -- Initial remainder of CSS parser.
  , initialToken    :: CssToken                  -- Initial current token.
  , dictionary      :: [(T.Text, propValueT)]    -- Used only in tests of enum and multi-enum.
  , finalRem        :: T.Text                    -- Final (after a single test) remainder of CSS parser.
  , finalToken      :: CssToken                  -- Final (after a single test) token.
  , expectedValue   :: Maybe propValueT          -- Expected value parsed from (parser+token) pair.
  , testedFunction  :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe propValueT)
  }




defaultTestData :: ((CssParser, CssToken) -> ((CssParser, CssToken), Maybe propValueT)) -> TestData propValueT
defaultTestData f = TestData { initialRem     = ""
                             , initialToken   = CssTokNone
                             , dictionary     = []
                             , finalRem       = ""
                             , finalToken     = CssTokNone
                             , expectedValue  = Nothing
                             , testedFunction = f
                             }




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
--
-- testFunction :: [TestData propValueT] -> T.Text
testFunction []     = ""
testFunction (x:xs) = if not success
                      then T.pack ("Got: " ++ show propertyValue ++ ", Expected: " ++ show expectedPropertyValue ++ "; pat' = " ++ (show parser') ++ (show token'))
                      else testFunction xs
  where
    expectedPropertyValue = expectedValue x

    pat = (defaultParserInBlock . initialRem $ x, initialToken x)

    ((parser', token'), propertyValue) = (testedFunction x) pat

    success = and [ expectedPropertyValue == propertyValue
                  , token' == finalToken x
                  , remainder parser' == finalRem x
                  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsEnum
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
enumTestDict = [ ("first",    EnumTestDataFirst)
               , ("second",   EnumTestDataSecond)
               , ("third",    EnumTestDataThird)
               , ("fourth",   EnumTestDataFourth)
               , ("fifth",    EnumTestDataFifth)
               ]




-- TODO: add tests of strings with capital letters after you verify expected
-- behaviour in CSS spec.
enumTestData :: [TestData EnumTestData]
enumTestData =
  [
    -- Success case. First element on the list matches.
    TestData { dictionary = enumTestDict
             , initialRem = "!important;",  initialToken = CssTokIdent "first"
             , finalRem   = "important;",   finalToken   = CssTokDelim '!'
             , expectedValue  = Just EnumTestDataFirst
             , testedFunction = interpretTokensAsEnum enumTestDict
             }

    -- Success case. Middle element on the list matches.
  , TestData { dictionary = enumTestDict
             , initialRem = "!important;",  initialToken = CssTokIdent "third"
             , finalRem   = "important;",   finalToken   = CssTokDelim '!'
             , expectedValue  = Just EnumTestDataThird
             , testedFunction = interpretTokensAsEnum enumTestDict
             }

    -- Success case. Last element on the list matches.
  , TestData { dictionary = enumTestDict
             , initialRem = "!important;",  initialToken = CssTokIdent "fifth"
             , finalRem   = "important;",   finalToken   = CssTokDelim '!'
             , expectedValue  = Just EnumTestDataFifth
             , testedFunction = interpretTokensAsEnum enumTestDict
             }

    -- Failure case: token not matching a dict. This may happen if input
    -- document supports newer CSS standard, and the implementation
    -- implements older CSS standard.
  , TestData { dictionary = enumTestDict
             , initialRem = "!important;",  initialToken = CssTokIdent "eight"
             , finalRem   = "!important;",  finalToken   = CssTokIdent "eight"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsEnum enumTestDict
             }

    -- Failure case: empty dict. Not going to happen in practice because that
    -- would be a coding error that would be caught by other tests, and would
    -- require a deliberate omission of dictionary in parser code. Such
    -- situation will not be triggered by incoming CSS data. But still this
    -- is an interesting case worth testing.
  , TestData { dictionary = []
             , initialRem = "!important;",  initialToken = CssTokIdent "first"
             , finalRem   = "!important;",  finalToken   = CssTokIdent "first"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsEnum []
             }

    -- Atypical data: empty string in token.
  , TestData { dictionary = []
             , initialRem = "!important;",  initialToken = CssTokIdent ""
             , finalRem   = "!important;",  finalToken   = CssTokIdent ""
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsEnum []
             }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsMultiEnum
-- --------------------------------------------------------------------------




data MultiEnumTestData = MultiEnumTestData
  { initialRem2    :: T.Text                             -- Initial remainder of CSS parser.
  , initialToken2  :: CssToken                           -- Initial current token.
  , dictionary2    :: [(T.Text, MultiEnumTestDataMultiEnum)]
  , finalRem2      :: T.Text                             -- Final (after a single test) remainder of CSS parser.
  , finalToken2    :: CssToken                           -- Final (after a single test) token.
  , expectedValue2 :: Maybe [MultiEnumTestDataMultiEnum] -- Expected value parsed from (parser+token) pair.
  }




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
multiEnumTestData :: [MultiEnumTestData]
multiEnumTestData =
  [
    -- Success case. All elements on the list are found in CSS input.
    --
    -- Declaration value ends with ';' here. Tokenizer will take the char but
    -- tested function will keep it as current token.
    MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "second third fourth fifth !important;", initialToken2 = CssTokIdent "first"
                      , finalRem2   = "important;",                            finalToken2   = CssTokDelim '!'
                      , expectedValue2 = Just [ MultiEnumTestDataMultiEnumFirst
                                              , MultiEnumTestDataMultiEnumSecond
                                              , MultiEnumTestDataMultiEnumThird
                                              , MultiEnumTestDataMultiEnumFourth
                                              , MultiEnumTestDataMultiEnumFifth
                                              ] }

    -- Success case.
    --
    -- Declaration value ends with '}' here. Tokenizer will take the char but
    -- tested function will keep it as current token.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "third fourth} selector", initialToken2 = CssTokIdent "second"
                      , finalRem2   = " selector",              finalToken2   = CssTokBraceCurlyClose
                      , expectedValue2 = Just [ MultiEnumTestDataMultiEnumSecond
                                              , MultiEnumTestDataMultiEnumThird
                                              , MultiEnumTestDataMultiEnumFourth
                                              ] }

    -- Success case.
    --
    -- Tokens in input text appear in order that does not match order of
    -- enums. Tested function should not care about the order.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "fifth first; selector", initialToken2 = CssTokIdent "second"
                      , finalRem2   = " selector",             finalToken2   = CssTokSemicolon
                      , expectedValue2 = Just [ MultiEnumTestDataMultiEnumSecond
                                              , MultiEnumTestDataMultiEnumFifth
                                              , MultiEnumTestDataMultiEnumFirst
                                              ] }


    -- Failure case.
    --
    -- Remainder tokens with values not on list of enums should result in Nothing.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "hello there first; selector", initialToken2 = CssTokIdent "second"
                      , finalRem2   = "hello there first; selector", finalToken2   = CssTokIdent "second"
                      , expectedValue2 = Nothing
                      }

    -- Failure case.
    --
    -- Current token with value not on list of enums should also result in Nothing.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "second third fourth; selector", initialToken2 = CssTokIdent "red"
                      , finalRem2   = "second third fourth; selector", finalToken2   = CssTokIdent "red"
                      , expectedValue2 = Nothing
                      }

    -- Failure case.
    --
    -- One of remainder tokens is a color, so the result is Nothing.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "hello #aabbcc first; selector", initialToken2 = CssTokIdent "second"
                      , finalRem2   = "hello #aabbcc first; selector", finalToken2   = CssTokIdent "second"
                      , expectedValue2 = Nothing
                      }

    -- Failure case.
    --
    -- Current token is a color, so the result is Nothing.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "second third fourth; selector", initialToken2 = CssTokIdent "#aabbcc"
                      , finalRem2   = "second third fourth; selector", finalToken2   = CssTokIdent "#aabbcc"
                      , expectedValue2 = Nothing
                      }

{-  -- FIXME: the tested function recognizes current token as valid token, but
    -- then it doesn't discard "12px" in remainder as something unexpected.
    -- The result of the test function is "Just
    -- [MultiEnumTestDataMultiEnumSecond]", while it probably should be
    -- Nothing.

    -- Failure case.
    --
    -- One of remainder tokens is a length, so the result is Nothing.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "12px !important", initialToken2 = CssTokIdent "second"
                      , finalRem2   = "12px !important", finalToken2   = CssTokIdent "second"
                      , expectedValue2 = Nothing
                      }
-}

    -- Failure case.
    --
    -- Current token is a length, so the result is Nothing.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "second third fourth; selector", initialToken2 = CssTokPerc . CssNumF $ 50.0
                      , finalRem2   = "second third fourth; selector", finalToken2   = CssTokPerc . CssNumF $ 50.0
                      , expectedValue2 = Nothing
                      }


    -- Failure case: empty dict. Not going to happen in practice because that
    -- would be a coding error that would be caught by other tests, and would
    -- require a deliberate omission of dictionary in parser code. Such
    -- situation will not be triggered by incoming CSS data. But still this
    -- is an interesting case worth testing.
  , MultiEnumTestData { dictionary2 = []
                      , initialRem2 = "first third; selector", initialToken2 = CssTokIdent "second"
                      , finalRem2   = "first third; selector", finalToken2   = CssTokIdent "second"
                      , expectedValue2 = Nothing
                      }


    -- Atypical data: empty string in token.
  , MultiEnumTestData { dictionary2 = multiEnumTestDict
                      , initialRem2 = "second third fourth fifth !important;", initialToken2 = CssTokIdent ""
                      , finalRem2   = "second third fourth fifth !important;", finalToken2   = CssTokIdent ""
                      , expectedValue2 = Nothing
                      }
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
multiEnumTestFunction :: [MultiEnumTestData] -> T.Text
multiEnumTestFunction []     = ""
multiEnumTestFunction (x:xs) = if not success
                               then T.pack ("Got: " ++ show propertyValue ++ ", Expected: " ++ show expectedPropertyValue ++ "; pat' = " ++ (show parser') ++ (show token'))
                               else multiEnumTestFunction xs
  where
    expectedPropertyValue = expectedValue2 x

    (pat', propertyValue) = interpretTokensAsMultiEnum (dictionary2 x) pat

    pat = (defaultParserInBlock . initialRem2 $ x, initialToken2 x)
    (parser', token') = pat'

    success = and [ expectedPropertyValue == propertyValue
                  , token' == finalToken2 x
                  , remainder parser' == finalRem2 x
                  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsAuto
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
autoTestData :: [TestData AutoTestValue]
autoTestData =
  [
    -- Success case. Current token is "auto".
    --
    -- Declaration value ends with ';' here. Tokenizer will take the char but
    -- tested function will keep it as current token.
    TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other", initialToken = CssTokIdent "auto"
             , finalRem   = "; other",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . AutoTestValueCtor $ CssDistanceAuto
             , testedFunction = interpretTokensAsAuto AutoTestValueCtor
             }

    -- Failure case. Current token is not "auto".
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other", initialToken = CssTokIdent "italic"
             , finalRem   = "something; other", finalToken   = CssTokIdent "italic"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsAuto AutoTestValueCtor
             }

    -- Failure case. Current token is definitely not "auto".
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other", initialToken = CssTokBraceCurlyClose
             , finalRem   = "something; other", finalToken   = CssTokBraceCurlyClose
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsAuto AutoTestValueCtor
             }

    -- Failure case. Current token is definitely not "auto".
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other", initialToken = CssTokDelim '@'
             , finalRem   = "something; other", finalToken   = CssTokDelim '@'
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsAuto AutoTestValueCtor
             }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsColor
-- --------------------------------------------------------------------------




-- An artifical value ctor for value of some artifical CSS property.
data ColorTestValue = ColorTestValueCtor Int
  deriving (Eq, Show)




colorTestData1 :: [TestData ColorTestValue]
colorTestData1 =
  [
    -- Success case. Color as hex.
    TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other1", initialToken = CssTokHash CssHashId "fb5"  -- fb5 interpreted as rgb should be expanded to rrggbb in form of ffbb55
             , finalRem   = "; other1",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0xffbb55
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success case. Color as hex.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other2", initialToken = CssTokHash CssHashId "12de56"
             , finalRem   = "; other2",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0x12de56
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success case. Color as hex. Capital letters in hex string.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other3", initialToken = CssTokHash CssHashId "12DE5A"
             , finalRem   = "; other3",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0x12de5A
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success case. Color as name.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other4", initialToken = CssTokIdent "red"
             , finalRem   = "; other4",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0xff0000
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success case. Color as less frequently used name.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other5", initialToken = CssTokIdent "antiquewhite"
             , finalRem   = "; other5",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0xfaebd7
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure case. Name is not a proper color name.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other6", initialToken = CssTokIdent "czerwony"
             , finalRem   = "something; other6", finalToken   = CssTokIdent "czerwony"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure, hash is not a hex-digit string.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other7", initialToken = CssTokHash CssHashId "ident"
             , finalRem   = "something; other7", finalToken   = CssTokHash CssHashId "ident"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other8", initialToken = CssTokHash CssHashId "a"
             , finalRem   = "something; other8", finalToken   = CssTokHash CssHashId "a"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other9", initialToken = CssTokHash CssHashId "ab"
             , finalRem   = "something; other9", finalToken   = CssTokHash CssHashId "ab"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success, just a sanity check.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other10", initialToken = CssTokHash CssHashId "abc"
             , finalRem   = "; other10",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0xaabbcc
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other11", initialToken = CssTokHash CssHashId "abcd"
             , finalRem   = "something; other11", finalToken   = CssTokHash CssHashId "abcd"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other12", initialToken = CssTokHash CssHashId "abcde"
             , finalRem   = "something; other12", finalToken   = CssTokHash CssHashId "abcde"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success, just a sanity check.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other13", initialToken = CssTokHash CssHashId "abcdef"
             , finalRem   = "; other13",          finalToken   = CssTokIdent "something"
             , expectedValue  = Just . ColorTestValueCtor $ 0xabcdef
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure, empty hash string.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other14", initialToken = CssTokHash CssHashId ""
             , finalRem   = "something; other14", finalToken   = CssTokHash CssHashId ""
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure. Empty ident string.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other15", initialToken = CssTokIdent ""
             , finalRem   = "something; other15", finalToken   = CssTokIdent ""
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure. Unexpected current token type.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other16", initialToken = CssTokDelim '@'
             , finalRem   = "something; other16", finalToken   = CssTokDelim '@'
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

  -- Failure. Unexpected current token type.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "something; other17", initialToken = CssTokPerc . CssNumF $ 50.0
             , finalRem   = "something; other17", finalToken   = CssTokPerc . CssNumF $ 50.0
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }
  ]




-- Input data is a rgb function
colorTestData2 :: [TestData ColorTestValue]
colorTestData2 =
  [
    -- Success case. Color as rgb function.
    TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "15,50,200); next-property1", initialToken = CssTokFunc "rgb"
             , finalRem   = " next-property1",            finalToken   = CssTokSemicolon
             , expectedValue  = Just . ColorTestValueCtor $ 0x0f32c8
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success case. Color as rgb function, with percentages.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "90%,20%,0%); next-property2", initialToken = CssTokFunc "rgb"
             , finalRem   = " next-property2",             finalToken   = CssTokSemicolon
             , expectedValue  = Just . ColorTestValueCtor $ 0xe63300
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Success case. Color as rgb function, with percentages.
    --
    -- Percentage values over 100% or under 0% should be clipped (in this
    -- case to 100%,0%,15%).
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "120%,-20%,15%); next-property3", initialToken = CssTokFunc "rgb"
             , finalRem   = " next-property3",                finalToken   = CssTokSemicolon
             , expectedValue  = Just . ColorTestValueCtor $ 0xff0026
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }

    -- Failure. Mix of dimensionless values and percentages should be rejected.
  , TestData { dictionary = [] -- unused in tests of this function
             , initialRem = "90%,20,0%); next-property4", initialToken = CssTokFunc "rgb"
             , finalRem   = "90%,20,0%); next-property4", finalToken   = CssTokFunc "rgb"
             , expectedValue  = Nothing
             , testedFunction = interpretTokensAsColor ColorTestValueCtor
             }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsStringList
-- --------------------------------------------------------------------------




-- An artifical value ctor for value of some artifical CSS property.
data StringListTestType = StringListTestCtor [T.Text]
  deriving (Eq, Show)




-- 'test data' item for 'string list' function.
tdsl = defaultTestData $ interpretTokensAsStringList StringListTestCtor




stringListTestData :: [TestData StringListTestType]
stringListTestData =
  [

    -- Success case.
    tdsl { initialRem    = ",tuesday, wednesday , thursday; next-property", initialToken = CssTokIdent "monday"
         , finalRem      = " next-property",                                finalToken   = CssTokSemicolon
         , expectedValue = Just . StringListTestCtor $ ["monday", "tuesday", "wednesday", "thursday"]
         , testedFunction = interpretTokensAsStringList StringListTestCtor
         }

    -- Success case.
  , tdsl { initialRem    = "; next-property",  initialToken = CssTokIdent "monday"
         , finalRem      = " next-property",   finalToken   = CssTokSemicolon
         , expectedValue = Just . StringListTestCtor $ ["monday"]
         , testedFunction = interpretTokensAsStringList StringListTestCtor
         }

    -- Failure case.
    --
    -- Hash token won't be interpreted as valid token for a list, and none of
    -- following tokens will.
  , tdsl { initialRem     = "tuesday; next-property",  initialToken = CssTokHash CssHashId "monday"
         , finalRem       = "tuesday; next-property",  finalToken   = CssTokHash CssHashId "monday"
         , expectedValue  = Nothing
         , testedFunction = interpretTokensAsStringList StringListTestCtor
         }

    -- Failure case.
    --
    -- Numeric values in input text should also lead to invalid parsing.
  , tdsl { initialRem     = "tuesday, 99redbaloons, wednesday; next-property",  initialToken = CssTokIdent "monday"
         , finalRem       = "tuesday, 99redbaloons, wednesday; next-property",  finalToken   = CssTokIdent "monday"
         , expectedValue  = Nothing
         , testedFunction = interpretTokensAsStringList StringListTestCtor
         }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsInteger
-- --------------------------------------------------------------------------




-- An artifical value ctor for value of some artifical CSS property.
data IntegerTestType = IntegerTestCtor Int
  deriving (Eq, Show)




-- 'test data' item for 'integer' function.
tdi = defaultTestData $ interpretTokensAsInteger IntegerTestCtor (0, 0)




integerTestData :: [TestData IntegerTestType]
integerTestData =
  [
    -- Success case. Numeric token with value within range.
    tdi { initialRem    = "!important",  initialToken = CssTokNum . CssNumI $ 9
        , finalRem      = "important",   finalToken   = CssTokDelim '!'
        , testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
        , expectedValue = Just . IntegerTestCtor $ 9
        }

    -- Success case. Numeric token with value within range.
  , tdi { initialRem    = "!important",  initialToken = CssTokNum . CssNumI $ -150
        , finalRem      = "important",   finalToken   = CssTokDelim '!'
        , testedFunction = interpretTokensAsInteger IntegerTestCtor (-200, 900)
        , expectedValue = Just . IntegerTestCtor $ (-150)
        }

    -- Success case. Numeric token with value that is equal to lower bound of
    -- range.
  , tdi { initialRem    = "!important",  initialToken = CssTokNum . CssNumI $ 0
        , finalRem      = "important",   finalToken   = CssTokDelim '!'
        , testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
        , expectedValue = Just . IntegerTestCtor $ 0
        }

    -- Success case. Numeric token with value that is equal to upper bound of
    -- range.
  , tdi { initialRem    = "!important",  initialToken = CssTokNum . CssNumI $ 900
        , finalRem      = "important",   finalToken   = CssTokDelim '!'
        , testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
        , expectedValue = Just . IntegerTestCtor $ 900
        }

    -- Failure case. Numeric token with value that is below lower bound of
    -- range.
  , tdi { initialRem    = "!important",  initialToken = CssTokNum . CssNumI $ 99
        , finalRem      = "!important",  finalToken   = CssTokNum . CssNumI $ 99
        , testedFunction = interpretTokensAsInteger IntegerTestCtor (100, 900)
        , expectedValue = Nothing
        }

    -- Failure case. Numeric token with value that is above lower bound of
    -- range.
  , tdi { initialRem    = "!important",  initialToken = CssTokNum . CssNumI $ 901
        , finalRem      = "!important",  finalToken   = CssTokNum . CssNumI $ 901
        , testedFunction = interpretTokensAsInteger IntegerTestCtor (100, 900)
        , expectedValue = Nothing
        }
  ]




{- -------------------------------------------------------------------------- -}




-- If some error is found, test function returns non-empty string which can
-- help identify a test that failed.
testCases =
  [
    TestCase (do assertEqual "manual tests of interpretTokensAsEnum"              "" (testFunction enumTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsMultiEnum"         "" (multiEnumTestFunction multiEnumTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsAuto"              "" (testFunction autoTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsColor (value)"     "" (testFunction colorTestData1))
  , TestCase (do assertEqual "manual tests of interpretTokensAsColor (rgb)"       "" (testFunction colorTestData2))
  , TestCase (do assertEqual "manual tests of interpretTokensAsStringList"        "" (testFunction stringListTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsInteger"           "" (testFunction integerTestData))
  ]




testsCssPropertyValue :: IO String
testsCssPropertyValue = do
  testCounts <- runTestTT (TestList (testCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.PropertyValue failed"

