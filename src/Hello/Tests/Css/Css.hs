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

import Hello.Css.Parser
import Css
import Hello.Tests.Css.Data
import Hello.Utils




-- On success return empty string. On failure return string representation of
-- selector, for which test failed.
specificityTest :: [(Int, CssSelector)] -> T.Text
specificityTest []     = ""
specificityTest (x:xs) = if expectedSpecificity /= (selectorSpecificity selector)
                         then T.pack . show $ selector
                         else specificityTest xs
  where
    expectedSpecificity = fst x
    selector = snd x




cssTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of specificity" "" (specificityTest specificityTestManualData))

  ]




testsCssCss :: IO String
testsCssCss = do
  counts <- runTestTT (TestList (cssTestCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsCssCss failed"





