{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Selector (runTests
                                )
where




import qualified Data.Text as T
import Test.HUnit
import System.Exit
import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser
import Hello.Utils




-- Tests of parseSelector function, most basic cases
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parseSelectorTestManualDataBasic = [
  -- parser's remainder before/after      expected selector

  -- Recognition of most basic case: no selector in input buffer.
    ( "0",              "",   Nothing )

  -- Recognition of most basic case: just "id" selector.
  , ( "#some_id",       "",   Just CssSelector {matchCacheOffset = (-1), simpleSelectors = [
                                                   CssSimpleSelector {selectorPseudoClass = [], selectorId = "some_id", selectorClass = [], selectorType = cssSimpleSelectorElementAny, combinator = CssCombinatorNone }
                                                   ]})

  -- Recognition of most basic case: just "class" selector.
  , ( ".some_class",    "",   Just CssSelector {matchCacheOffset = (-1), simpleSelectors = [
                                                   CssSimpleSelector {selectorPseudoClass = [], selectorId = "", selectorClass = ["some_class"], selectorType = cssSimpleSelectorElementAny, combinator = CssCombinatorNone }
                                                   ]})

  -- Recognition of most basic case: just "pseudo class" selector.
  , ( ":link",          "",   Just CssSelector {matchCacheOffset = (-1), simpleSelectors = [
                                                   CssSimpleSelector {selectorPseudoClass = ["link"], selectorId = "", selectorClass = [], selectorType = cssSimpleSelectorElementAny, combinator = CssCombinatorNone }
                                                   ]})
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parseSelectorTest :: [(T.Text, T.Text, Maybe CssSelector)] -> T.Text
parseSelectorTest []     = ""
parseSelectorTest (x:xs) = if expectedSelector /= selector || remainderAfter /= (remainder p1)
                           then remainderBefore
                           else parseSelectorTest xs
  where
    remainderBefore  = triplet1st x
    remainderAfter   = triplet2nd x
    expectedSelector = triplet3rd x

    -- Both cases should work the same. If current token is None, tested
    -- function should get some non-None input token.
    ((p1, t1), selector) = parseSelector (defaultParser{remainder = remainderBefore}, CssTokNone)
    --((p1, t1), selector) = parseSelector $ nextToken1 defaultParser{remainder = remainderBefore}




selectorTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of parseSelector - basic cases" "" (parseSelectorTest parseSelectorTestManualDataBasic))
  ]




runTests :: IO ()
runTests = do
  counts <- runTestTT (TestList (selectorTestCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure

