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
import Hello.Css.Parser.Selector
import Hello.Css.Selector
import Hello.Utils
import Hello.Ffi.Css.SelectorLink -- CssComplexSelectorLink; TODO: the FFI code should not be imported in tests, at least not in this test.





linkAndChainTestData :: [[CssComplexSelectorLink]]
linkAndChainTestData =
  [
    -- Single link
    [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = ["link"]
                                                            , selectorId = ""
                                                            , selectorClass = []
                                                            , selectorTagName = CssTypeSelectorUniversal}
                            , combinator = Nothing}]


    -- Two links
  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["topnav-container"]
                                                            , selectorTagName = CssTypeSelectorUniversal}
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
                                                            , selectorTagName = CssTypeSelectorUniversal}
                            , combinator = Just CssCombinatorAdjacentSibling}]


  -- Three links
  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["pure-menu-horizontal"]
                                                            , selectorTagName = CssTypeSelectorUniversal}
                            , combinator = Nothing},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["pure-menu-has-children"]
                                                            , selectorTagName = CssTypeSelectorUniversal}
                            , combinator = Just CssCombinatorDescendant},
     CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = ["after"]
                                                            , selectorId = ""
                                                            , selectorClass = ["pure-menu-link"]
                                                            , selectorTagName = CssTypeSelectorUniversal}
                            , combinator = Just CssCombinatorDescendant}]


  , [CssComplexSelectorLink {compound = CssCompoundSelector { selectorPseudoClass = []
                                                            , selectorId = ""
                                                            , selectorClass = ["navmenu"]
                                                            , selectorTagName = CssTypeSelectorUniversal}
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
                          then T.pack ("in = " ++ show (linkIn x) ++ ", chain = " ++ show (linksToChain x) ++ ", out = " ++ show (linkOut x))
                          else linkAndChainTest xs
  where
    linkIn l  = l
    linkOut l = chainToLinks (linksToChain l) []




selectorTestCases :: [Test]
selectorTestCases =
  [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
    TestCase (do
                  assertEqual "manual tests of link and chain"                     "" (linkAndChainTest linkAndChainTestData))
  ]




testsCssComplexSelector :: IO String
testsCssComplexSelector = do
  testCounts <- runTestTT (TestList selectorTestCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] testsCssComplexSelector failed"

