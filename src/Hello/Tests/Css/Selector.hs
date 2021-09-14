{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Selector
  (
    testsCssComplexSelector
  )
where




import qualified Data.Text as T
import Test.HUnit
import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser
import Hello.Utils




-- Tests of parseComplexSelector function, most basic cases
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parseComplexSelectorTestManualDataBasic = [
  -- parser's remainder before/after      expected selector

  -- Recognition of most basic case: no selector in input buffer.
    ( "0",              "",   Nothing )

  -- Recognition of most basic case: just "id" selector.
  , ( "#some_id",       "",   Just CssComplexSelector
                              { matchCacheOffset = (-1)
                              , links = [CssComplexSelectorLink { compound = CssCompoundSelector2{selectorPseudoClass = [], selectorId = "some_id", selectorClass = [], selectorTagName = CssTypeSelectorUniv}, combinator = CssCombinatorNone }]})

  -- Recognition of most basic case: just "class" selector.
  , ( ".some_class",    "",   Just CssComplexSelector
                              { matchCacheOffset = (-1)
                              , links = [CssComplexSelectorLink {compound = CssCompoundSelector2{selectorPseudoClass = [], selectorId = "", selectorClass = ["some_class"], selectorTagName = CssTypeSelectorUniv}, combinator = CssCombinatorNone }]})

  -- Recognition of most basic case: just "pseudo class" selector.
  , ( ":link",          "",   Just CssComplexSelector
                              { matchCacheOffset = (-1)
                              , links = [CssComplexSelectorLink {compound = CssCompoundSelector2{selectorPseudoClass = ["link"], selectorId = "", selectorClass = [], selectorTagName = CssTypeSelectorUniv}, combinator = CssCombinatorNone }]})
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parseComplexSelectorTest :: [(T.Text, T.Text, Maybe CssComplexSelector)] -> T.Text
parseComplexSelectorTest []     = ""
parseComplexSelectorTest (x:xs) = if expectedSelector /= cplxSel || remainderAfter /= (remainder p1)
                                  then remainderBefore
                                  else parseComplexSelectorTest xs
  where
    remainderBefore  = triplet1st x
    remainderAfter   = triplet2nd x
    expectedSelector = triplet3rd x

    -- Both cases should work the same. If current token is None, tested
    -- function should get some non-None input token.
    ((p1, t1), cplxSel) = parseComplexSelector (defaultParser{remainder = remainderBefore}, CssTokNone)
    --((p1, t1), cplxSel) = parseComplexSelector $ nextToken1 defaultParser{remainder = remainderBefore}




selectorTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of parseComplexSelector - basic cases" "" (parseComplexSelectorTest parseComplexSelectorTestManualDataBasic))
  ]




testsCssComplexSelector :: IO String
testsCssComplexSelector = do
  counts <- runTestTT (TestList (selectorTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssComplexSelector failed"

