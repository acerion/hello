{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.SelectorMatch
  (
    testsCssSelectorMatch
  )
where




import qualified Data.Text as T
import Test.HUnit
--import Debug.Trace

import Hello.Css.Selector
import Hello.Css.SelectorMatch
import Hello.Html.DoctreeNode
import Hello.Html.Tag

import qualified Hello.Tests.Utils.Hunit as H.H




data CompoundTestData = CompoundTestData
  { inCompound :: CssCompoundSelector
  , inDtn      :: DoctreeNode
  , matchFn    :: CssCompoundSelector -> DoctreeNode -> Bool
  , expected   :: Bool
  }




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
compoundTestFunction :: [CompoundTestData] -> [T.Text]
compoundTestFunction []     = []
compoundTestFunction (x:xs) = if expected x /= result
                              then [errMsg]
                              else compoundTestFunction xs
  where
    result = (matchFn x) (inCompound x) (inDtn x)
    errMsg = T.pack . unwords $
      [ "\n*** expected   = " ++ (show . expected $ x)
      , "\n*** result     = " ++ (show result)
      , "\n*** inCompound = " ++ (show . inCompound $ x)
      , "\n*** inDtn      = " ++ (show . inDtn $ x)
      ]




{- -------------------------------------------------------------------------- -}




-- Testcases of matching for pseudo-class simple selector that is a part of
-- compound selector.
compoundPseudoClassTestData :: [CompoundTestData]
compoundPseudoClassTestData =
  [
    -- Matching on pseudo-class: expected to match because pseudo-class is
    -- empty in both cases.
    CompoundTestData {
      inCompound = CssCompoundSelector { selectorTagName = CssTypeSelectorUniversal, selectorClass = [], selectorId = "", selectorPseudoClass = []}
    , inDtn      = defaultDoctreeNode { htmlElementIdx = htmlTagIndex "body", selClass = [], selId = "", selPseudoClass = []}
    , matchFn    = matchOnPseudoClass
    , expected   = True
    }

    -- Matching on pseudo-class: expected to fail because pseudo-class of dtn
    -- is empty, while pseudo-class of selector is not. This means that rule
    -- with given selector does not apply to given document node.
  , CompoundTestData {
      inCompound = CssCompoundSelector { selectorTagName = CssTypeSelectorUniversal, selectorClass = [], selectorId = "", selectorPseudoClass = ["visited"]}
    , inDtn      = defaultDoctreeNode { htmlElementIdx = htmlTagIndex "body", selClass = [], selId = "", selPseudoClass = []}
    , matchFn    = matchOnPseudoClass
    , expected   = False
    }

    -- Matching on pseudo-class: expected to succeed because pseudo-class of
    -- dtn is the same as pseudo-class of selector. This means that rule with
    -- given selector applies to given document node.
  , CompoundTestData {
      inCompound = CssCompoundSelector { selectorTagName = CssTypeSelectorUniversal, selectorClass = [], selectorId = "", selectorPseudoClass = ["visited"]}
    , inDtn      = defaultDoctreeNode { htmlElementIdx = htmlTagIndex "body", selClass = [], selId = "", selPseudoClass = ["visited"]}
    , matchFn    = matchOnPseudoClass
    , expected   = True
    }

    -- Matching on pseudo-class: expected to succeed because pseudo-class of
    -- dtn is the same as pseudo-class of selector. This means that rule with
    -- given selector applies to given document node.
    --
    -- This time there are two pseudo-class values in dtn and in selector.
  , CompoundTestData {
      inCompound = CssCompoundSelector { selectorTagName = CssTypeSelectorUniversal, selectorClass = [], selectorId = "", selectorPseudoClass = ["visited","hover"]}
    , inDtn      = defaultDoctreeNode { htmlElementIdx = htmlTagIndex "body", selClass = [], selId = "", selPseudoClass = ["visited","hover"]}
    , matchFn    = matchOnPseudoClass
    , expected   = True
    }

    -- Matching on pseudo-class: expected to succeed because pseudo-class of
    -- dtn is the same as pseudo-class of selector. This means that rule with
    -- given selector applies to given document node.
    --
    -- This time there are two pseudo-class values in dtn and in selector,
    -- but the values are in different order.
  , CompoundTestData {
      inCompound = CssCompoundSelector { selectorTagName = CssTypeSelectorUniversal, selectorClass = [], selectorId = "", selectorPseudoClass = ["visited","hover"]}
    , inDtn      = defaultDoctreeNode { htmlElementIdx = htmlTagIndex "body", selClass = [], selId = "", selPseudoClass = ["hover","visited"]}
    , matchFn    = matchOnPseudoClass
    , expected   = True
    }

    -- TODO: add test cases for situations, when length of pseudo-class is
    -- different in selector and in dtn (including empty pseudo-class list).
  ]




{- -------------------------------------------------------------------------- -}




testCases :: [Test]
testCases =
  [
    -- If some error is found, test function returns some data (e.g. non-empty
    -- string or test index) which can help identify which test failed.
    TestCase (do H.H.assertSuccess "manual tests of matchOnPseudoClass"      (compoundTestFunction compoundPseudoClassTestData))
  ]




testsCssSelectorMatch :: IO String
testsCssSelectorMatch = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.SelectorMatch failed"


