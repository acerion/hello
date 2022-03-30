{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Html.Attribute
  (
    testsHtmlAttribute
  )
where




import Test.HUnit
import qualified Data.Text as T

import Hello.Html.Attribute
import Hello.Css.Parser




lengthData =
  --  attribute token               length
  [
    ("100",                         Just (100.0, cssLengthTypePX))
  , ("100.1",                       Just (100.1, cssLengthTypePX))
  , ("100%",                        Just (100.0, cssLengthTypePercentage))
  , ("100.1%",                      Just (100.1, cssLengthTypePercentage))

  , ("",                            Nothing)
  , ("bird",                        Nothing)
  , ("100#",                        Nothing)
  ]




lengthTest :: [(T.Text, Maybe (Float, Int))] -> T.Text
lengthTest []     = ""
lengthTest (x:xs) = if len x == parseLengthOrMultiLength (inAttribute x)
                    then lengthTest xs
                    else (inAttribute x)
  where
    inAttribute (x, _) = x
    len         (_, y) = y




testCases = [
    TestCase(assertEqual "valid length tests"           "" (lengthTest lengthData))
  ]




testsHtmlAttribute :: IO String
testsHtmlAttribute = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsHtmlAttribute failed"

