{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser
  (
    testsCssParser
  )
where




import qualified Data.Text as T
import qualified Data.Sequence as S

import Test.HUnit
import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Parser
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils




{- -------------------------------------------------------------------------- -}




{-
Test a function that parses a CSS declaration: a property name + property
value. Tested function can return multiple CSS declarations for property
names such as "background" or "border".
-}




parseDeclarationTestData =
  [
    ( "background-color: inherit",               [CssDeclWrapper { property = CssDeclarationBackgroundColor CssBackgroundColorInherit,        important = False } ])
  , ( "background-color: inherit !important",    [CssDeclWrapper { property = CssDeclarationBackgroundColor CssBackgroundColorInherit,        important = True  } ])
  , ( "background-color: blue",                  [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssBackgroundColor 0x0000ff),    important = False } ])
  , ( "background-color: blue !important",       [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssBackgroundColor 0x0000ff),    important = True  } ])
  , ( "background-color: blue !important;",      [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssBackgroundColor 0x0000ff),    important = True  } ])
  , ( "background-color: #00ff00;",              [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssBackgroundColor 0x00ff00),    important = False } ])
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
parseDeclarationTest :: [(T.Text, [CssDeclWrapper])] -> T.Text
parseDeclarationTest []     = ""
parseDeclarationTest (x:xs) = if expectedDeclarations /= declarations
                              then T.pack (show declarations ++ " != " ++ show expectedDeclarations)
                              else parseDeclarationTest xs
  where
    rem                  = fst x
    expectedDeclarations = snd x
    ((parser', token'), declarations) = parseDeclaration (nextToken2 defaultParser { remainder = rem })




{- -------------------------------------------------------------------------- -}




testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of parseDeclaration" "" (parseDeclarationTest parseDeclarationTestData))
  ]




testsCssParser :: IO String
testsCssParser = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Tests.Css.Parser failed"

