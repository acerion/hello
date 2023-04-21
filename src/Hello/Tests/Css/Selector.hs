{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Selector
  (
    testsCssComplexSelector
  )
where




--import qualified Data.Text as T
import Test.HUnit
--import Debug.Trace

--import Hello.Css.Selector


selectorTestCases :: [Test]
selectorTestCases =
  [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.

  ]




testsCssComplexSelector :: IO String
testsCssComplexSelector = do
  testCounts <- runTestTT (TestList selectorTestCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] testsCssComplexSelector failed"

