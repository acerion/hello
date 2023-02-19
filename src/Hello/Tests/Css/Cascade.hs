{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Cascade
  (
    testsCssCascade
  )
where



-- import Debug.Trace
import Test.HUnit
import qualified Data.Text as T

import Hello.Css.Cascade
import Hello.Tests.Css.CascadeTestData




cssGetMinSpecStateTestFunction :: [GetMinSpecStateTestData] -> T.Text
cssGetMinSpecStateTestFunction []     = ""
cssGetMinSpecStateTestFunction (x:xs) = if output /= expectedOutput
                                        then T.pack ("output = " ++ show output ++ ", expectedOutput  = " ++ show expectedOutput)
                                        else cssGetMinSpecStateTestFunction xs
  where
    input          = matchingRules x
    output         = cssGetMinSpecState input
    expectedOutput = specificityState x




testCases :: [Test]
testCases =
  [
    -- If some error is found, test function returns some data (e.g. non-empty
    -- string or test index) which can help identify which test failed.
    TestCase (do assertEqual "manual tests of cssGetMinSpecState"                     "" (cssGetMinSpecStateTestFunction cssGetMinSpecStateTestData))
  ]




testsCssCascade :: IO String
testsCssCascade = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Tests.Css.Cascade failed"

