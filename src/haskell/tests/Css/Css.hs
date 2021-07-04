{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module TestsCss (testsCss)
where




import qualified Data.Text as T
import Test.HUnit
import System.Exit
import CssParser
import Css
import TestsCssData
import HelloUtils




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




testsCss :: IO ()
testsCss = do
  counts <- runTestTT (TestList (cssTestCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure





