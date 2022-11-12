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



-- propValueT: type of value of a CSS property, e.g. CssValueBackgroundColor
data TestData propValueT = TestData
  { inputPat       :: (CssParser, CssToken) -- initial parser+token
  , expectedResult :: Maybe ((CssParser, CssToken), propValueT)
  , testedFunction :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), propValueT)
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
    result = (testedFunction x) (inputPat x)

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
    success (Just (pat1, val1)) (Just (pat2, val2)) = and [ (remainder . fst $ pat1) == (remainder . fst $ pat2) -- Parsers
                                                          , snd pat1 == snd pat2                                 -- Tokens
                                                          , val1 == val2                                         -- Parsed values
                                                          ]
    success _ _             = False




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
    TestData { testedFunction = interpretTokensAsEnum enumTestDict
             , inputPat       =       (defaultParserInBlock "!important;", CssTokIdent "first")
             , expectedResult = Just ((defaultParserInBlock "important;" , CssTokDelim '!'), EnumTestDataFirst)
             }

    -- Success case. Middle element on the list matches.
  , TestData { testedFunction = interpretTokensAsEnum enumTestDict
             , inputPat       =       (defaultParserInBlock "!important;", CssTokIdent "third")
             , expectedResult = Just ((defaultParserInBlock "important;",  CssTokDelim '!'), EnumTestDataThird)
             }

    -- Success case. Last element on the list matches.
  , TestData { testedFunction = interpretTokensAsEnum enumTestDict
             , inputPat       =       (defaultParserInBlock "!important;", CssTokIdent "fifth")
             , expectedResult = Just ((defaultParserInBlock "important;",  CssTokDelim '!'), EnumTestDataFifth)
             }

    -- Failure case: token not matching a dict. This may happen if input
    -- document supports newer CSS standard, and the implementation
    -- implements older CSS standard.
  , TestData { testedFunction = interpretTokensAsEnum enumTestDict
             , inputPat       = (defaultParserInBlock "!important;", CssTokIdent "eight")
             , expectedResult = Nothing
             }

    -- Failure case: empty dict. Not going to happen in practice because that
    -- would be a coding error that would be caught by other tests, and would
    -- require a deliberate omission of dictionary in parser code. Such
    -- situation will not be triggered by incoming CSS data. But still this
    -- is an interesting case worth testing.
  , TestData { testedFunction = interpretTokensAsEnum []
             , inputPat       = (defaultParserInBlock "!important;", CssTokIdent "first")
             , expectedResult = Nothing
             }

    -- Atypical data: empty string in token.
  , TestData { testedFunction = interpretTokensAsEnum enumTestDict
             , inputPat       = (defaultParserInBlock "!important;", CssTokIdent "")
             , expectedResult = Nothing
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
    TestData { testedFunction = interpretTokensAsAuto AutoTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other", CssTokIdent "auto")
             , expectedResult = Just ((defaultParserInBlock "; other",          CssTokIdent "something"), AutoTestValueCtor CssDistanceAuto)
             }

    -- Failure case. Current token is not "auto".
  , TestData { testedFunction = interpretTokensAsAuto AutoTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other", CssTokIdent "italic")
             , expectedResult = Nothing
             }

    -- Failure case. Current token is definitely not "auto".
  , TestData { testedFunction = interpretTokensAsAuto AutoTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other", CssTokBraceCurlyClose)
             , expectedResult = Nothing
             }

    -- Failure case. Current token is definitely not "auto".
  , TestData { testedFunction = interpretTokensAsAuto AutoTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other", CssTokDelim '@')
             , expectedResult = Nothing
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
    TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other1", CssTokHash CssHashId "fb5")  -- fb5 interpreted as rgb should be expanded to rrggbb in form of ffbb55
             , expectedResult = Just ((defaultParserInBlock "; other1",          CssTokIdent "something"), ColorTestValueCtor 0xffbb55)
             }

    -- Success case. Color as hex.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other2", CssTokHash CssHashId "12de56")
             , expectedResult = Just ((defaultParserInBlock "; other2",         CssTokIdent "something"), ColorTestValueCtor 0x12de56)
             }

    -- Success case. Color as hex. Capital letters in hex string.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other3", CssTokHash CssHashId "12DE5A")
             , expectedResult = Just ((defaultParserInBlock "; other3",          CssTokIdent "something"), ColorTestValueCtor 0x12de5A)
             }

    -- Success case. Color as name.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other4", CssTokIdent "red")
             , expectedResult = Just ((defaultParserInBlock "; other4",          CssTokIdent "something"), ColorTestValueCtor 0xff0000)
             }

    -- Success case. Color as less frequently used name.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other5", CssTokIdent "antiquewhite")
             , expectedResult = Just ((defaultParserInBlock "; other5",          CssTokIdent "something"), ColorTestValueCtor 0xfaebd7)
             }

    -- Failure case. Name is not a proper color name.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other6", CssTokIdent "czerwony")
             , expectedResult = Nothing
             }

    -- Failure, hash is not a hex-digit string.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other7", CssTokHash CssHashId "ident")
             , expectedResult = Nothing
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other8", CssTokHash CssHashId "a")
             , expectedResult = Nothing
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other9", CssTokHash CssHashId "ab")
             , expectedResult = Nothing
             }

    -- Success, just a sanity check.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other10", CssTokHash CssHashId "abc")
             , expectedResult = Just ((defaultParserInBlock "; other10",          CssTokIdent "something"), ColorTestValueCtor 0xaabbcc)
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other11", CssTokHash CssHashId "abcd")
             , expectedResult = Nothing
             }

    -- Failure, hash has incorrect count of digits (should be either 3 or 6).
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other12", CssTokHash CssHashId "abcde")
             , expectedResult = Nothing
             }

    -- Success, just a sanity check.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "something; other13", CssTokHash CssHashId "abcdef")
             , expectedResult = Just ((defaultParserInBlock "; other13",          CssTokIdent "something"), ColorTestValueCtor 0xabcdef)
             }

    -- Failure, empty hash string.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other14", CssTokHash CssHashId "")
             , expectedResult = Nothing
             }

    -- Failure. Empty ident string.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other15", CssTokIdent "")
             , expectedResult = Nothing
             }

    -- Failure. Unexpected current token type.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other16", CssTokDelim '@')
             , expectedResult = Nothing
             }

  -- Failure. Unexpected current token type.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "something; other17", CssTokPerc . CssNumF $ 50.0)
             , expectedResult = Nothing
             }
  ]




-- Input data is a rgb function
colorTestData2 :: [TestData ColorTestValue]
colorTestData2 =
  [
    -- Success case. Color as rgb function.
    TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "15,50,200); next-property1",        CssTokFunc "rgb")
             , expectedResult = Just ((defaultParserInBlock " next-property1", CssTokSemicolon), ColorTestValueCtor 0x0f32c8)
             }

    -- Success case. Color as rgb function, with percentages.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "90%,20%,0%); next-property2",       CssTokFunc "rgb")
             , expectedResult = Just ((defaultParserInBlock " next-property2", CssTokSemicolon), ColorTestValueCtor 0xe63300)
             }

    -- Success case. Color as rgb function, with percentages.
    --
    -- Percentage values over 100% or under 0% should be clipped (in this
    -- case to 100%,0%,15%).
  , TestData  { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       =       (defaultParserInBlock "120%,-20%,15%); next-property3", CssTokFunc "rgb")
             , expectedResult = Just ((defaultParserInBlock " next-property3",                CssTokSemicolon), ColorTestValueCtor 0xff0026)
             }

    -- Failure. Mix of dimensionless values and percentages should be rejected.
  , TestData { testedFunction = interpretTokensAsColor ColorTestValueCtor
             , inputPat       = (defaultParserInBlock "90%,20,0%); next-property4", CssTokFunc "rgb")
             , expectedResult = Nothing
             }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsStringList
-- --------------------------------------------------------------------------




-- An artifical value ctor for value of some artifical CSS property.
data StringListTestType = StringListTestCtor [T.Text]
  deriving (Eq, Show)




stringListTestData :: [TestData StringListTestType]
stringListTestData =
  [

    -- Success case.
    TestData { testedFunction = interpretTokensAsStringList StringListTestCtor
             , inputPat       =       (defaultParserInBlock ",tuesday, wednesday , thursday; next-property", CssTokIdent "monday")
             , expectedResult = Just ((defaultParserInBlock " next-property",                                CssTokSemicolon), StringListTestCtor ["monday", "tuesday", "wednesday", "thursday"])
             }

    -- Success case.
  , TestData { testedFunction = interpretTokensAsStringList StringListTestCtor
             , inputPat       =       (defaultParserInBlock "; next-property", CssTokIdent "monday")
             , expectedResult = Just ((defaultParserInBlock " next-property",  CssTokSemicolon), StringListTestCtor ["monday"])
             }

    -- Failure case.
    --
    -- Hash token won't be interpreted as valid token for a list, and none of
    -- following tokens will.
  , TestData { testedFunction = interpretTokensAsStringList StringListTestCtor
             , inputPat       = (defaultParserInBlock "tuesday; next-property", CssTokHash CssHashId "monday")
             , expectedResult = Nothing
             }

    -- Failure case.
    --
    -- Numeric values in input text should also lead to invalid parsing.
  , TestData { testedFunction = interpretTokensAsStringList StringListTestCtor
             , inputPat       = (defaultParserInBlock "tuesday, 99redbaloons, wednesday; next-property", CssTokIdent "monday")
             , expectedResult = Nothing
             }
  ]




-- --------------------------------------------------------------------------
-- Tests of interpretTokensAsInteger
-- --------------------------------------------------------------------------




-- An artifical value ctor for value of some artifical CSS property.
data IntegerTestType = IntegerTestCtor Int
  deriving (Eq, Show)




integerTestData :: [TestData IntegerTestType]
integerTestData =
  [
    -- Success case. Numeric token with value within range.
    TestData { testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
             , inputPat       =       (defaultParserInBlock"!important", CssTokNum . CssNumI $ 9)
             , expectedResult = Just ((defaultParserInBlock "important", CssTokDelim '!'), IntegerTestCtor 9)
             }

    -- Success case. Numeric token with value within range.
  , TestData { testedFunction = interpretTokensAsInteger IntegerTestCtor (-200, 900)
             , inputPat       =       (defaultParserInBlock "!important", CssTokNum . CssNumI $ -150)
             , expectedResult = Just ((defaultParserInBlock "important", CssTokDelim '!'), IntegerTestCtor (-150))
             }

    -- Success case. Numeric token with value that is equal to lower bound of
    -- range.
  , TestData { testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
             , inputPat       =       (defaultParserInBlock "!important", CssTokNum . CssNumI $ 0)
             , expectedResult = Just ((defaultParserInBlock "important",  CssTokDelim '!'), IntegerTestCtor 0)
             }

    -- Success case. Numeric token with value that is equal to upper bound of
    -- range.
  , TestData { testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
             , inputPat       =       (defaultParserInBlock "!important", CssTokNum . CssNumI $ 900)
             , expectedResult = Just ((defaultParserInBlock "important",  CssTokDelim '!'), IntegerTestCtor 900)
             }

    -- Failure case. Numeric token with value that is below lower bound of
    -- range.
  , TestData { testedFunction = interpretTokensAsInteger IntegerTestCtor (100, 900)
             , inputPat       = (defaultParserInBlock "!important", CssTokNum . CssNumI $ 99)
             , expectedResult = Nothing
             }

    -- Failure case. Numeric token with value that is above lower bound of
    -- range.
  , TestData { testedFunction = interpretTokensAsInteger IntegerTestCtor (0, 900)
             , inputPat       = (defaultParserInBlock "!important", CssTokNum . CssNumI $ 901)
             , expectedResult = Nothing
             }
  ]




{- -------------------------------------------------------------------------- -}




-- If some error is found, test function returns non-empty string which can
-- help identify a test that failed.
testCases =
  [
    TestCase (do assertEqual "manual tests of interpretTokensAsEnum"              "" (testFunction enumTestData))
  , TestCase (do assertEqual "manual tests of interpretTokensAsMultiEnum"         "" (testFunction multiEnumTestData))
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

