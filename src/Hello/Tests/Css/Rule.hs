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
--import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser.Declaration
import Hello.Css.Parser.Rule
import Hello.Css.Parser.Selector
import Hello.Css.Selector
import Hello.Html.Tag




-- Test of getTopCompound function
--
-- "top compound selector" is the rightmost compound selector in a rule, and data
-- structure representing it is storing it at the end of a list. This test is
-- meant to ensure that even if we change the data structure, the tested
-- function will always return the right compound selector.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
getTopCompoundTestManualDataBasic = [
  -- parser's input remainder      expected top simple selector

    ( "body {color: black;background-color: #ffffff;padding:0px;}"
    , defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} )

  , ( ".pure-g > div {-webkit-box-sizing: border-box;-moz-box-sizing: border-box;box-sizing: border-box;}"
    , defaultCssCompoundSelector{ selectorTagName = CssTypeSelector . htmlTagIndex $ "div"} )

  , ( ".navmenu li:hover > ul {display: block;}"
    , defaultCssCompoundSelector{ selectorTagName = CssTypeSelector . htmlTagIndex $ "ul"} )

  , ( ".pure-menu-horizontal .pure-menu-has-children .pure-menu-link:after{content:\"x\"}"
    , defaultCssCompoundSelector{ selectorClass = ["pure-menu-link"], selectorPseudoClass = ["after"]} )

    -- This is a modification of the previous test case. Here we have only
    -- one compound selector, and it consists of 3 class selectors.
  , ( ".pure-menu-horizontal.pure-menu-has-children.pure-menu-link{content:\"x\"}"
    , defaultCssCompoundSelector{ selectorClass = ["pure-menu-horizontal", "pure-menu-has-children", "pure-menu-link"]} )

  , ( "H3.SummaryHL + #id { margin: 0px; }"
    , defaultCssCompoundSelector{ selectorId = "id"} )

  , ( ".topnav-container a:visited { color: DarkBlue; }"
    , defaultCssCompoundSelector{ selectorPseudoClass = ["visited"], selectorTagName = CssTypeSelector . htmlTagIndex $ "a"} )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
getTopCompoundTest :: [(T.Text, CssCompoundSelector)] -> T.Text
getTopCompoundTest []     = ""
getTopCompoundTest (x:xs) = if expectedCompound /= cpd
                            then T.pack $ show remainderIn ++ ", expected " ++ show expectedCompound ++ ", got " ++ show cpd
                            else getTopCompoundTest xs
  where
    remainderIn  = fst x
    expectedCompound = snd x

    (_, selectorList) = readSelectorList (defaultParser remainderIn, CssTokNone)
    cpd = case selectorList of
            Just l  -> getTopCompound CssRule { complexSelector = head l
                                              , declarationSet  = defaultCssDeclarationSet
                                              , specificity     = 0
                                              , position        = 0
                                              }
            Nothing -> defaultCssCompoundSelector




ruleTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of getTopCompound" "" (getTopCompoundTest getTopCompoundTestManualDataBasic))
  ]




testsCssRule :: IO String
testsCssRule = do
  testCounts <- runTestTT (TestList (ruleTestCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] testsCssRule failed"

