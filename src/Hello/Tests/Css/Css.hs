{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Css
  (
    testsCssCss
  )
where




import qualified Data.Text as T
import Test.HUnit

import Hello.Css.Cascade
import Hello.Css.Parser
import Hello.Css.Selector
import Hello.Css.SelectorMatch
import Hello.Html.DoctreeNode
import Hello.Utils

import Hello.Tests.Css.Data
import Hello.Tests.Css.Match.Data





-- On success return empty string. On failure return string representation of
-- selector, for which test failed.
specificityTest :: [(Int, CssComplexSelector)] -> T.Text
specificityTest []     = ""
specificityTest (x:xs) = if expectedSpecificity /= (selectorSpecificity complex)
                         then T.pack . show $ complex
                         else specificityTest xs
  where
    expectedSpecificity = fst x
    complex = snd x



-- On success return empty string. On failure return string representation of
-- selector, for which test failed.
matchTest :: [(CssCompoundSelectorMatch, CssCompoundSelector, DoctreeNode)] -> T.Text
matchTest []     = ""
matchTest (x:xs) = if expectedMatch x /= (compoundSelectorMatches' (cpdSel x) (dtn x))
                         then T.pack ((show $ cpdSel x) ++ "    @@@@    " ++ (show $ dtn x))
                         else matchTest xs
  where
    expectedMatch (a, _, _) = a
    cpdSel        (_, b, _) = b
    dtn           (_, _, c) = c




cssTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
       TestCase (do
                 assertEqual "manual tests of specificity" "" (specificityTest specificityTestManualData))

     , TestCase (do
                 assertEqual "manual tests of matching" ""    (matchTest matchTestManualData))

  ]




testsCssCss :: IO String
testsCssCss = do
  counts <- runTestTT (TestList (cssTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssCss failed"





