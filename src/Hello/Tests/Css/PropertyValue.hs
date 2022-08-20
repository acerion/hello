{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.PropertyValue
  (
    testsCssPropertyValue
  )
where




import qualified Data.Text as T

import Debug.Trace

import Test.HUnit

import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils





{- -------------------------------------------------------------------------- -}




data EnumTestData = EnumTestData
  { initialRem    :: T.Text                          -- Initial remainder of CSS parser.
  , initialToken  :: CssToken                        -- Initial current token.
  , dictionary    :: [(T.Text, EnumTestDataEnum)]
  , finalRem      :: T.Text                          -- Final (after a single test) remainder of CSS parser.
  , finalToken    :: CssToken                        -- Final (after a single test) token.
  , expectedValue :: Maybe EnumTestDataEnum          -- Expected value parsed from (parser+token) pair.
  }




-- An artifical set of values of some artifical CSS property.
data EnumTestDataEnum
  = EnumTestDataEnumFirst
  | EnumTestDataEnumSecond
  | EnumTestDataEnumThird
  | EnumTestDataEnumFourth
  | EnumTestDataEnumFifth
  deriving (Enum, Eq, Show)




-- The test code will be testing if parsing a string token will result in
-- proper Haskell enum value. This dict is specifying the mapping from the
-- string to enum.
enumTestDict = [ ("first",    EnumTestDataEnumFirst)
               , ("second",   EnumTestDataEnumSecond)
               , ("third",    EnumTestDataEnumThird)
               , ("fourth",   EnumTestDataEnumFourth)
               , ("fifth",    EnumTestDataEnumFifth)
               ]




-- TODO: add tests of strings with capital letters after you verify expected
-- behaviour in CSS spec.
enumTestData :: [EnumTestData]
enumTestData =
  [
    -- Success case. First element on the list matches.
    EnumTestData { dictionary = enumTestDict
                 , initialRem = "!important;",  initialToken = CssTokIdent "first"
                 , finalRem   = "important;",   finalToken   = CssTokDelim '!'
                 , expectedValue = Just EnumTestDataEnumFirst }

    -- Success case. Middle element on the list matches.
  , EnumTestData { dictionary = enumTestDict
                 , initialRem = "!important;",  initialToken = CssTokIdent "third"
                 , finalRem   = "important;",   finalToken   = CssTokDelim '!'
                 , expectedValue = Just EnumTestDataEnumThird }

    -- Success case. Last element on the list matches.
  , EnumTestData { dictionary = enumTestDict
                 , initialRem = "!important;",  initialToken = CssTokIdent "fifth"
                 , finalRem   = "important;",   finalToken   = CssTokDelim '!'
                 , expectedValue = Just EnumTestDataEnumFifth }

    -- Failure case: token not matching a dict. This may happen if input
    -- document supports newer CSS standard, and the implementation
    -- implements older CSS standard.
  , EnumTestData { dictionary = enumTestDict
                 , initialRem = "!important;",  initialToken = CssTokIdent "eight"
                 , finalRem   = "!important;",  finalToken   = CssTokIdent "eight"
                 , expectedValue = Nothing }

    -- Failure case: empty dict. Not going to happen in practice because that
    -- would be a coding error that would be caught by other tests, and would
    -- require a deliberate omission of dictionary in parser code. Such
    -- situation will not be triggered by incoming CSS data. But still this
    -- is an interesting case worth testing.
  , EnumTestData { dictionary = []
                 , initialRem = "!important;",  initialToken = CssTokIdent "first"
                 , finalRem   = "!important;",  finalToken   = CssTokIdent "first"
                 , expectedValue = Nothing }

    -- Atypical data: empty string in token.
  , EnumTestData { dictionary = []
                 , initialRem = "!important;",  initialToken = CssTokIdent ""
                 , finalRem   = "!important;",  finalToken   = CssTokIdent ""
                 , expectedValue = Nothing }
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
enumTestFunction :: [EnumTestData] -> T.Text
enumTestFunction []     = ""
enumTestFunction (x:xs) = if not success
                          then T.pack ("Got: " ++ show propertyValue ++ ", Expected: " ++ show expectedPropertyValue ++ "; pat' = " ++ (show parser') ++ (show token'))
                          else enumTestFunction xs
  where
    expectedPropertyValue = expectedValue x

    vs :: ValueState3 EnumTestDataEnum
    vs = (defaultValueState3 pat) { dict = dictionary x }

    (vs', propertyValue) = interpretTokensAsEnum vs

    pat = (defaultParser{remainder = initialRem x}, initialToken x)
    (parser', token') = pt3 vs'

    success = and [ expectedPropertyValue == propertyValue
                  , token' == finalToken x
                  , remainder parser' == finalRem x
                  ]




{- -------------------------------------------------------------------------- -}




testCases = [
  -- If some error is found, test function returns non-empty string which can
  -- help identify which test failed.
     TestCase (do assertEqual "manual tests of interpretTokensAsEnum"  "" (enumTestFunction enumTestData))
  ]




testsCssPropertyValue :: IO String
testsCssPropertyValue = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.PropertyValue failed"

