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




--import Debug.Trace
import Test.HUnit
import qualified Data.Text as T

import Hello.Css.Distance

import Hello.Html.Attribute
import Hello.Html.Doctype




lengthData =
  --  attribute token               resulting length
  [
    ("100",                         Just (100.0, cssLengthTypePX))
  , ("100.1",                       Just (100.1, cssLengthTypePX))

  -- when lengths are in percentage, the main code divides percentage by 100
  , ("100%",                        Just (1.0, cssLengthTypePercentage))
  , ("100.1%",                      Just (1.001, cssLengthTypePercentage))

  , ("",                            Nothing)
  , ("bird",                        Nothing)
  , ("100#",                        Nothing) -- No "garbage" allowed after a seemingly valid value
  ]




lengthTest :: [(T.Text, Maybe (Float, Int))] -> T.Text
lengthTest []     = ""
lengthTest (x:xs) = if len x == parseLengthOrMultiLength (inAttribute x)
                    then lengthTest xs
                    else (inAttribute x)
  where
    inAttribute (x, _) = x
    len         (_, y) = y




{- -------------------------------------------------------------------------- -}




nameOrIdValuesData =
  [
    -- HTML4 tests
    ( HtmlDoctypeHtml 4.0, "name01", "some_name", True  )
  , ( HtmlDoctypeHtml 4.0, "name02", "Some_name", True  )
  , ( HtmlDoctypeHtml 4.0, "name03", "Some_na44", True  )

  , ( HtmlDoctypeHtml 4.0, "name04", "4ome_name", False ) -- value can't start with something other than a letter
  , ( HtmlDoctypeHtml 4.0, "name05", "&ome_name", False ) -- value can't start with something other than a letter
  , ( HtmlDoctypeHtml 4.0, "name06", "some?name", False ) -- value can't contain '?' character
  , ( HtmlDoctypeHtml 4.0, "name07", "some name", False ) -- value can't contain ' ' character
  , ( HtmlDoctypeHtml 4.0, "name08", " ome_name", False ) -- value can't contain ' ' character

  , ( HtmlDoctypeHtml 4.0, "name09", "",          False ) -- value can't be empty
  , ( HtmlDoctypeHtml 4.0, "name10", " ",         False ) -- value can't be just a space


    -- HTML5 tests
  , ( HtmlDoctypeHtml 5.0, "name51", "some_name", True  )
  , ( HtmlDoctypeHtml 5.0, "name52", "Some_name", True  )
  , ( HtmlDoctypeHtml 5.0, "name53", "Some_na44", True  )

  , ( HtmlDoctypeHtml 5.0, "name54", "4ome_na44", True  ) -- for HTML5 we allow value to start with number

  , ( HtmlDoctypeHtml 5.0, "name55", "",          False ) -- value can't be empty
  , ( HtmlDoctypeHtml 5.0, "name56", " ",         False ) -- value can't be just a space
  ]




nameOrIdValuesTest :: [(HtmlDoctype, T.Text, T.Text, Bool)] -> T.Text
nameOrIdValuesTest []     = ""
nameOrIdValuesTest (x:xs) = if expected x == validateNameOrIdValue (doctype x) (attrName x) (attrValue x)
                            then nameOrIdValuesTest xs
                            else (attrName x)
  where
    doctype   (a, _, _, _) = a
    attrName  (_, b, _, _) = b
    attrValue (_, _, c, _) = c
    expected  (_, _, _, d) = d




{- -------------------------------------------------------------------------- -}




testCases = [
    TestCase(assertEqual "valid length tests"           "" (lengthTest lengthData))
  , TestCase(assertEqual "name or id values"            "" (nameOrIdValuesTest nameOrIdValuesData))
  ]




testsHtmlAttribute :: IO String
testsHtmlAttribute = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Html.Attribute failed"


