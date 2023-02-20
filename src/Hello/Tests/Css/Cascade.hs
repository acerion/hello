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




testCases :: [Test]
testCases =
  [
  ]




testsCssCascade :: IO String
testsCssCascade = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Tests.Css.Cascade failed"

