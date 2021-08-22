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




-- Test of getTopSimSel function
--
-- "top simple selector" is the rightmost simple selector in a rule, and data
-- structure representing it is storing it at the end of a list. This test is
-- meant to ensure that even if we change the data structure, the tested
-- function will always return the right simple selector.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
getTopSimSelTestManualDataBasic = [
  -- parser's input remainder      expected top simple selector

    ( "body {color: black;background-color: #ffffff;padding:0px;}"
    , defaultSimpleSelector{ selectorType = htmlTagIndex "body", combinator = CssCombinatorNone } )

  , ( ".pure-g > div {-webkit-box-sizing: border-box;-moz-box-sizing: border-box;box-sizing: border-box;}"
    , defaultSimpleSelector{ selectorType = htmlTagIndex "div", combinator = CssCombinatorChild } )

  , ( ".navmenu li:hover > ul {display: block;}"
    , defaultSimpleSelector{ selectorType = htmlTagIndex "ul", combinator = CssCombinatorChild } )

  , ( ".pure-menu-horizontal .pure-menu-has-children .pure-menu-link:after{content:\"x\"}"
    , defaultSimpleSelector{ selectorClass = ["pure-menu-link"], selectorPseudoClass = ["after"], combinator = CssCombinatorDescendant} )

  , ( "H3.SummaryHL + #id { margin: 0px; }"
    , defaultSimpleSelector{ selectorId = "id", combinator = CssCombinatorAdjacentSibling} )

  , ( ".topnav-container a:visited { color: DarkBlue; }"
    , defaultSimpleSelector{ selectorPseudoClass = ["visited"], selectorType = htmlTagIndex "a", combinator = CssCombinatorDescendant } )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
getTopSimSelTest :: [(T.Text, CssSimpleSelector)] -> T.Text
getTopSimSelTest []     = ""
getTopSimSelTest (x:xs) = if expectedSimSel /= simSel
                          then remainderIn
                          else getTopSimSelTest xs
  where
    remainderIn    = fst x
    expectedSimSel = snd x

    -- Both cases should work the same. If current token is None, tested
    -- function should get some non-None input token.
    ((p1, t1), selectorList) = readSelectorList (defaultParser{remainder = remainderIn}, CssTokNone)
    rule = CssRule { selector = head selectorList, declarationSet = defaultCssDeclarationSet, specificity = 0, position = 0 }
    simSel = getTopSimSel rule




ruleTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of getTopSimSel" "" (getTopSimSelTest getTopSimSelTestManualDataBasic))
  ]




testsCssRule :: IO String
testsCssRule = do
  counts <- runTestTT (TestList (ruleTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssRule failed"

