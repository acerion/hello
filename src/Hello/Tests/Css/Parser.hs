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
    ( "background-color: inherit",               [CssDeclWrapper { property = CssDeclarationBackgroundColor CssValueBackgroundColorInherit,        important = False } ])
  , ( "background-color: inherit !important",    [CssDeclWrapper { property = CssDeclarationBackgroundColor CssValueBackgroundColorInherit,        important = True  } ])
  , ( "background-color: blue",                  [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColor 0x0000ff),    important = False } ])
  , ( "background-color: blue !important",       [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColor 0x0000ff),    important = True  } ])
  , ( "background-color: blue !important;",      [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColor 0x0000ff),    important = True  } ])
  , ( "background-color: #00ff00;",              [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColor 0x00ff00),    important = False } ])
  , ( "background-color: 0x00ff00;",             []) -- Invalid format of HEX value

  , ( "border-top-color: inherit",                        [CssDeclWrapper { property = CssDeclarationBorderTopColor   CssValueBorderColorInherit,       important = False } ])
  , ( "border-top-color: transparent",                    [CssDeclWrapper { property = CssDeclarationBorderTopColor   CssValueBorderColorTransparent,   important = False } ])
  , ( "border-top-color: red",                            [CssDeclWrapper { property = CssDeclarationBorderTopColor   $ CssValueBorderColor 0xff0000,   important = False } ])
  , ( "border-top-color: #0000ff !important",             [CssDeclWrapper { property = CssDeclarationBorderTopColor   $ CssValueBorderColor 0x0000ff,   important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-top-color: #0000ff !iportant",              [CssDeclWrapper { property = CssDeclarationBorderTopColor   $ CssValueBorderColor 0x0000ff,   important = False } ])

  , ( "border-right-color: inherit",                      [CssDeclWrapper { property = CssDeclarationBorderRightColor CssValueBorderColorInherit,       important = False } ])
  , ( "border-right-color: transparent",                  [CssDeclWrapper { property = CssDeclarationBorderRightColor CssValueBorderColorTransparent,   important = False } ])
  , ( "border-right-color: lime !important",              [CssDeclWrapper { property = CssDeclarationBorderRightColor $ CssValueBorderColor 0x00ff00,   important = True  } ])
  , ( "border-right-color: rgb(255, 0, 0) !important",    [CssDeclWrapper { property = CssDeclarationBorderRightColor $ CssValueBorderColor 0xff0000,   important = True  } ])
   -- Testing for parsing of bad css: space after function name.
  , ( "border-right-color: rgb (255, 0, 0) !important",   [])

  , ( "border-bottom-color: inherit !important",          [CssDeclWrapper { property = CssDeclarationBorderBottomColor CssValueBorderColorInherit,      important = True  } ])
  , ( "border-bottom-color: transparent",                 [CssDeclWrapper { property = CssDeclarationBorderBottomColor CssValueBorderColorTransparent,  important = False } ])
  , ( "border-bottom-color: pink",                        [CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0xffc0cb,  important = False } ])
  , ( "border-bottom-color: rgb(0, 255, 0) !important",   [CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0x00ff00,  important = True  } ])
    -- Testing for parsing of bad css: typo in property name.
  , ( "border-bottom_color: rgb(0, 255, 0) !important",   [])

  , ( "border-left-color: inherit",                       [CssDeclWrapper { property = CssDeclarationBorderLeftColor CssValueBorderColorInherit,        important = False } ])
  , ( "border-left-color: transparent !important",        [CssDeclWrapper { property = CssDeclarationBorderLeftColor CssValueBorderColorTransparent,    important = True  } ])
  , ( "border-left-color: purple",                        [CssDeclWrapper { property = CssDeclarationBorderLeftColor $ CssValueBorderColor 0x800080,    important = False } ])
  , ( "border-left-color: rgb(0, 0, 255) !important",     [CssDeclWrapper { property = CssDeclarationBorderLeftColor $ CssValueBorderColor 0x0000ff,    important = True  } ])
    -- Testing for parsing of bad css: invalid value name.
  , ( "border-left-color: purpe",                         [])

  , ( "color: inherit",                          [CssDeclWrapper { property = CssDeclarationColor CssValueColorInherit,        important = False } ])
  , ( "color: inherit !important",               [CssDeclWrapper { property = CssDeclarationColor CssValueColorInherit,        important = True  } ])
  , ( "color: red",                              [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0xff0000),    important = False } ])
  , ( "color: lime !important",                  [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0x00ff00),    important = True  } ]) -- Yes, "lime" not "green".
  , ( "color: blue !important;",                 [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0x0000ff),    important = True  } ])
  , ( "color: #abcdef;",                         [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0xabcdef),    important = False } ])
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
parseDeclarationTest :: [(T.Text, [CssDeclWrapper])] -> T.Text
parseDeclarationTest []     = ""
parseDeclarationTest (x:xs) = if expectedDeclarations /= declarations
                              then T.pack ("Got: " ++ show declarations ++ ", Expected: " ++ show expectedDeclarations)
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

