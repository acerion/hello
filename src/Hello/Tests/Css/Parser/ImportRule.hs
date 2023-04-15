{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser.ImportRule
  (
    testsCssParserImportRule
  )
where




import qualified Data.Text as T
import Test.HUnit
--import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser.Rule
import Hello.Css.Parser.Value
import Hello.Css.Rule
import Hello.Utils.Parser

import qualified Hello.Tests.Utils.Hunit as H.H




{- -------------------------------------------------------------------------- -}




data TestData = TestData
  { remainderIn       :: T.Text     -- ^ Input remainder to be parsed and
                                    -- turned into CSS rule (into rule's
                                    -- ingredients).
  , resultExpected    :: Maybe ((CssParser, CssToken), CssRule2) -- ^ Expected output of tested function.
  } deriving (Show, Eq)




-- Testcases of parserImportRule function
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
testData :: [TestData]
testData =
  [
    -- Success case: 'url' function, a full URL without quotes.
    TestData { remainderIn       = "@import url(http://the.best.com);"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 33 }, CssTokEnd)
                                        , CssImportRule $ ParsedUrl "http://the.best.com")
             }

    -- Success case: 'url' function, a full URL with quotes.
    --
    -- TODO: check if quoted URL is a valid case.
  , TestData { remainderIn       = "@import url(\"http://the.best.com\");"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 35 }, CssTokEnd)
                                        , CssImportRule $ ParsedUrl "http://the.best.com")
             }

    -- Success case: 'url' function, URL is just an unquoted file name.
    --
    -- TODO: how to turn the relative URL into non-relative one?
  , TestData { remainderIn       = "@import url(mystyle.css);"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 25 }, CssTokEnd)
                                        , CssImportRule $ ParsedUrl "mystyle.css")
             }

    -- Success case: 'url' function, URL is just a quoted file name.
    --
    -- TODO: how to turn the relative URL into non-relative one?
    --
    -- TODO: check if quoted URL is a valid case.
  , TestData { remainderIn       = "@import url(\"mystyle.css\");"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 27 }, CssTokEnd)
                                        , CssImportRule $ ParsedUrl "mystyle.css")
             }

    -- Success case: URL as quoted string (<string>), URL is just a quoted
    -- file name.
    --
    -- TODO: how to turn the relative URL into non-relative one?
  , TestData { remainderIn       = "@import \"mystyle.css\";"
             , resultExpected    = Just (((defaultParser "") { bufOffset = 22 }, CssTokEnd)
                                        , CssImportRule $ ParsedUrl "mystyle.css")
             }

    -- Failure case: URL as unquoted string. A <string> must be quoted by
    -- spec in order to be considered valid.
  , TestData { remainderIn       = "@import mystyle.css;"
             , resultExpected    = Nothing
             }

    -- Failure case: import rule, but without terminating ';'.
  , TestData { remainderIn       = "@import url(\"http://the.best.com\")"
             , resultExpected    = Nothing
             }

    -- Failure case: import rule, but without terminating ';'.
  , TestData { remainderIn       = "@import \"mystyle.css\""
             , resultExpected    = Nothing
             }

    -- Failure case: style rule won't be parsed as import rule.
  , TestData { remainderIn       = "body {color: #003412;background-color: #ffff00; line-height: normal;}"
             , resultExpected    = Nothing
             }

    -- TODO: add failure case when trying to parse @media rule as @import
    -- rule.
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
    result = runParser parserImportRule (startTokenizer parser)
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
    TestCase (do H.H.assertSuccess "manual tests of parserImportRule"      (testFunction testData))
  ]




testsCssParserImportRule :: IO String
testsCssParserImportRule = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.ImportRule failed"


