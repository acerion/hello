{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Rule
  (
    testsCssRule
  )
where




import qualified Data.Text as T
import Test.HUnit
import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser
import Hello.Utils
import HtmlTag




-- Test of getTopLink function
--
-- "top compound selector" is the rightmost compound selector in a rule, and data
-- structure representing it is storing it at the end of a list. This test is
-- meant to ensure that even if we change the data structure, the tested
-- function will always return the right compound selector.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
getTopLinkTestManualDataBasic = [
  -- parser's input remainder      expected top simple selector

    ( "body {color: black;background-color: #ffffff;padding:0px;}"
    , defaultComplexSelectorLink{ selectorTagName = CssTypeSelector . htmlTagIndex $ "body", combinator = CssCombinatorNone } )

  , ( ".pure-g > div {-webkit-box-sizing: border-box;-moz-box-sizing: border-box;box-sizing: border-box;}"
    , defaultComplexSelectorLink{ selectorTagName = CssTypeSelector . htmlTagIndex $ "div", combinator = CssCombinatorChild } )

  , ( ".navmenu li:hover > ul {display: block;}"
    , defaultComplexSelectorLink{ selectorTagName = CssTypeSelector . htmlTagIndex $ "ul", combinator = CssCombinatorChild } )

  , ( ".pure-menu-horizontal .pure-menu-has-children .pure-menu-link:after{content:\"x\"}"
    , defaultComplexSelectorLink{ selectorClass = ["pure-menu-link"], selectorPseudoClass = ["after"], combinator = CssCombinatorDescendant} )

  , ( "H3.SummaryHL + #id { margin: 0px; }"
    , defaultComplexSelectorLink{ selectorId = "id", combinator = CssCombinatorAdjacentSibling} )

  , ( ".topnav-container a:visited { color: DarkBlue; }"
    , defaultComplexSelectorLink{ selectorPseudoClass = ["visited"], selectorTagName = CssTypeSelector . htmlTagIndex $ "a", combinator = CssCombinatorDescendant } )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
getTopLinkTest :: [(T.Text, CssComplexSelectorLink)] -> T.Text
getTopLinkTest []     = ""
getTopLinkTest (x:xs) = if expectedLink /= link
                        then remainderIn
                        else getTopLinkTest xs
  where
    remainderIn  = fst x
    expectedLink = snd x

    -- Both cases should work the same. If current token is None, tested
    -- function should get some non-None input token.
    ((p1, t1), selectorList) = readSelectorList (defaultParser{remainder = remainderIn}, CssTokNone)
    rule = CssRule { complexSelector = head selectorList, declarationSet = defaultCssDeclarationSet, specificity = 0, position = 0 }
    link = getTopLink rule




ruleTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of getTopLink" "" (getTopLinkTest getTopLinkTestManualDataBasic))
  ]




testsCssRule :: IO String
testsCssRule = do
  counts <- runTestTT (TestList (ruleTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssRule failed"

