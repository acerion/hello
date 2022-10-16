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
--import Debug.Trace

import Hello.Chain
import Hello.Css.Tokenizer
import Hello.Css.Parser.Rule
import Hello.Css.Selector
import Hello.Utils
import Hello.Ffi.Css.SelectorLink -- CssComplexSelectorLink; TODO: the FFI code should not be imported in tests, at least not in this test.




-- Tests of parseComplexSelector function, most basic cases
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parseComplexSelectorTestManualDataBasic = [
  -- parser's remainder before/after      expected selector

  -- Recognition of most basic case: no selector in input buffer.
    ( "0",              "",   Nothing )

  -- Recognition of most basic case: just "id" selector.
  , ( "#some_id",       "",   Just CssCachedComplexSelector
                              { matchCacheOffset = (-1)
                              , chain = Last CssCompoundSelector { selectorPseudoClass = []
                                                                 , selectorId = "some_id"
                                                                 , selectorClass = []
                                                                 , selectorTagName = CssTypeSelectorUniv
                                                                 }
                              })

  -- Recognition of most basic case: just "class" selector.
  , ( ".some_class",    "",   Just CssCachedComplexSelector
                              { matchCacheOffset = (-1)
                              , chain = Last CssCompoundSelector { selectorPseudoClass = []
                                                                 , selectorId = ""
                                                                 , selectorClass = ["some_class"]
                                                                 , selectorTagName = CssTypeSelectorUniv
                                                                 }
                              })

  -- Recognition of most basic case: just "pseudo class" selector.
  , ( ":link",          "",   Just CssCachedComplexSelector
                              { matchCacheOffset = (-1)
                              , chain = Last CssCompoundSelector { selectorPseudoClass = ["link"]
                                                                 , selectorId = ""
                                                                 , selectorClass = []
                                                                 , selectorTagName = CssTypeSelectorUniv
                                                                 }
                              })
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parseComplexSelectorTest :: [(T.Text, T.Text, Maybe CssCachedComplexSelector)] -> T.Text
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
    ((p1, _t1), cplxSel) = parseComplexSelector (defaultParser{remainder = remainderBefore}, CssTokNone)
    --((p1, t1), cplxSel) = parseComplexSelector $ nextToken1 defaultParser{remainder = remainderBefore}





linkAndChainTestData =
  [
    -- Single link
    [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = ["link"]
                                                            , selectorId = ""
                                                            , selectorClass = []
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Nothing}]


    -- Two links
  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["topnav-container"]
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Nothing},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = ["visited"]
                                                            , selectorId = ""
                                                            , selectorClass = []
                                                            , selectorTagName = CssTypeSelector 0}
                            , combinator = Just CssCombinatorDescendant}]



  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["SummaryHL"]
                                                            , selectorTagName = CssTypeSelector 35}
                            , combinator = Nothing},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = "id"
                                                            , selectorClass = []
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Just CssCombinatorAdjacentSibling}]


  -- Three links
  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["pure-menu-horizontal"]
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Nothing},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["pure-menu-has-children"]
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Just CssCombinatorDescendant},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = ["after"]
                                                            , selectorId = ""
                                                            , selectorClass = ["pure-menu-link"]
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Just CssCombinatorDescendant}]


  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["navmenu"]
                                                            , selectorTagName = CssTypeSelectorUniv}
                            , combinator = Nothing},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = ["hover"]
                                                            , selectorId = ""
                                                            , selectorClass = []
                                                            , selectorTagName = CssTypeSelector 50}
                            , combinator = Just CssCombinatorDescendant},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = []
                                                            , selectorTagName = CssTypeSelector 85}
                            , combinator = Just CssCombinatorChild}]
  ]


-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
linkAndChainTest :: [[CssComplexSelectorLink]] -> T.Text
linkAndChainTest []     = ""
linkAndChainTest (x:xs) = if linkIn x /= linkOut x
                          then T.pack $ ("in = " ++ (show $ linkIn x) ++ ", chain = " ++ (show $ linksToChain x) ++ ", out = " ++ (show $ linkOut x))
                          else linkAndChainTest xs
  where
    linkIn l  = l
    linkOut l = chainToLinks (linksToChain l) []





selectorTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                  assertEqual "manual tests of parseComplexSelector - basic cases" "" (parseComplexSelectorTest parseComplexSelectorTestManualDataBasic))


   , TestCase (do
                  assertEqual "manual tests of link and chain"                     "" (linkAndChainTest linkAndChainTestData))
  ]




testsCssComplexSelector :: IO String
testsCssComplexSelector = do
  testCounts <- runTestTT (TestList (selectorTestCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] testsCssComplexSelector failed"

