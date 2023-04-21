{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser.MediaRule
  (
    testsCssParserMediaRule
  )
where




import qualified Data.Text as T
import qualified Data.Sequence as S
import Test.HUnit
--import Debug.Trace

import Hello.Chain
import Hello.Css.Declaration
import Hello.Css.Tokenizer
import Hello.Css.Parser.Property
import Hello.Css.Parser.Rule
import Hello.Css.Rule
import Hello.Css.Selector
import Hello.Utils.Parser

import qualified Hello.Tests.Utils.Hunit as H.H




{- -------------------------------------------------------------------------- -}




data TestData = TestData
  { remainderIn       :: T.Text     -- ^ Input remainder to be parsed and
                                    -- turned into CSS rule (into rule's
                                    -- ingredients).
  , resultExpected    :: Maybe ((CssParser, CssToken), CssRule2) -- ^ Expected output of tested function.
  } deriving (Show, Eq)



-- Definitions of expected parsed rules are in top level to avoid too much
-- nesting of code.
styleRule1 :: CssRule2
styleRule1 = CssStyleRule CssRule { complexSelector = [WrapCompound $ CssCompoundSelector { selectorPseudoClass = []
                                                                                          , selectorId = ""
                                                                                          , selectorClass = []
                                                                                          , selectorTagName = CssTypeSelector 33}]
                                  , declarationSet = CssDeclarationSet { isSafe = True
                                                                       , items = S.fromList [CssDeclaration { property = CssPropertyColor (CssValueColor 0), important = False}]}
                                  , specificity = 1
                                  , position = 0}
             False




-- Testcases of parserMediaRule parser
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
testData :: [TestData]
testData =
  [
    -- Success case: simple media rule with simple media query and simple style sheet.
    TestData { remainderIn       = "@media all { h1{color: black} }"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 31 }, CssTokEnd)
                                        , CssMediaRule [[CssTokIdent "all"]] [styleRule1])
             }

    -- Success case: simple media rule followed by style rule - to confirm
    -- that parser of media rule doesn't do anyting crazy.
  , TestData { remainderIn       = "@media all { h1{color: black} }h2{width: 10px}"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 33, remainder = "{width: 10px}" }, CssTokIdent "h2")
                                        , CssMediaRule [[CssTokIdent "all"]] [styleRule1])
             }

    -- Success case: simple media rule followed by invalid import rule - to confirm
    -- that parser of media rule doesn't do anyting crazy.
  , TestData { remainderIn       = "@media all { h1{color: black} } @import url(\"style.css\")"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 39, remainder = " url(\"style.css\")" }, CssTokAtKeyword "import")
                                        , CssMediaRule [[CssTokIdent "all"]] [styleRule1])
             }

    -- Success case: simple media rule followed by another media rule - to
    -- confirm that parser of media rule doesn't do anyting crazy.
  , TestData { remainderIn       = "@media all { h1{color: black} }@media all {div{width:20px}}"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 37, remainder = " all {div{width:20px}}" }, CssTokAtKeyword "media")
                                        , CssMediaRule [[CssTokIdent "all"]] [styleRule1])
             }

    -- Success case: simple media rule followed by misplaced closing bracke -
    -- to confirm that parser of media rule doesn't do anyting crazy.
  , TestData { remainderIn       = "@media all { h1{color: black} }}@media all {div{width:40px}}"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 32, remainder = "@media all {div{width:40px}}" }, CssTokBraceCurlyClose)
                                        , CssMediaRule [[CssTokIdent "all"]] [styleRule1])
             }



    -- Failure case: missing brace before style sheet.
  , TestData { remainderIn       = "@media h1{color: black} }"
             , resultExpected    = Nothing
             }

{- TODO: enable this test.
    -- Failure case: too many opening braces before style sheet.
  , TestData { remainderIn       = "@media all { { h1{color: black} }"
             , resultExpected    = Nothing
             }
-}

    -- Failure case: missing style sheet.
  , TestData { remainderIn       = "@media all"
             , resultExpected    = Nothing
             }
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
testFunction :: [TestData] -> [T.Text]
testFunction []     = []
testFunction (x:xs) = if resultExpected x /= result
                      then [errMsg]
                      else testFunction xs
  where
    parser = defaultParser . remainderIn $ x
    result = runParser parserMediaRule (startTokenizer parser)
    errMsg = T.pack . unwords $
      [ "\n*** resultExpected    = " ++ (show . resultExpected $ x)
      , "\n*** result            = " ++ (show result)
      , "\n*** remainerIn        = " ++ (show . remainderIn $ x)
      ]




testCases :: [Test]
testCases =
  [
    -- If some error is found, test function returns some data (e.g. non-empty
    -- string or test index) which can help identify which test failed.
    TestCase (do H.H.assertSuccess "manual tests of parserMediaRule"      (testFunction testData))
  ]




testsCssParserMediaRule :: IO String
testsCssParserMediaRule = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.MediaRule failed"


