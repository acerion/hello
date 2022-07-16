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
import Hello.Css.Distance
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




  , ( "border-top-style: none !important",       [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleNone,          important = True  } ])
  , ( "border-top-style: hidden",                [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleHidden,        important = False } ])
  , ( "border-top-style: dotted !important",     [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleDotted,        important = True  } ])
  , ( "border-top-style: dashed",                [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleDashed,        important = False } ])
  , ( "border-top-style: solid !important",      [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleSolid,         important = True  } ])
  , ( "border-top-style: double",                [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleDouble,        important = False } ])
  , ( "border-top-style: groove !important",     [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleGroove,        important = True  } ])
  , ( "border-top-style: ridge",                 [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleRidge,         important = False } ])
  , ( "border-top-style: inset !important",      [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleInset,         important = True  } ])
  , ( "border-top-style: outset",                [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleOutset,        important = False } ])
  , ( "border-top-style: inherit !important",    [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleInherit,       important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-top-style: inherit !mportant",     [CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleInherit,       important = False } ])

  , ( "border-right-style: none",                [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleNone,        important = False } ])
  , ( "border-right-style: hidden !important",   [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleHidden,      important = True  } ])
  , ( "border-right-style: dotted",              [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleDotted,      important = False } ])
  , ( "border-right-style: dashed !important",   [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleDashed,      important = True  } ])
  , ( "border-right-style: solid",               [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleSolid,       important = False } ])
  , ( "border-right-style: double !important",   [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleDouble,      important = True  } ])
  , ( "border-right-style: groove",              [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleGroove,      important = False } ])
  , ( "border-right-style: ridge !important",    [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleRidge,       important = True  } ])
  , ( "border-right-style: inset",               [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleInset,       important = False } ])
  , ( "border-right-style: outset !important",   [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleOutset,      important = True  } ])
  , ( "border-right-style: inherit",             [CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleInherit,     important = False } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-right-style: blue",                [])

  , ( "border-bottom-style: none !important",    [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleNone,       important = True  } ])
  , ( "border-bottom-style: hidden",             [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleHidden,     important = False } ])
  , ( "border-bottom-style: dotted !important",  [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleDotted,     important = True  } ])
  , ( "border-bottom-style: dashed",             [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleDashed,     important = False } ])
  , ( "border-bottom-style: solid !important",   [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleSolid,      important = True  } ])
  , ( "border-bottom-style: double",             [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleDouble,     important = False } ])
  , ( "border-bottom-style: groove !important",  [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleGroove,     important = True  } ])
  , ( "border-bottom-style: ridge",              [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleRidge,      important = False } ])
  , ( "border-bottom-style: inset !important",   [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleInset,      important = True  } ])
  , ( "border-bottom-style: outset",             [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleOutset,     important = False } ])
  , ( "border-bottom-style: inherit !important", [CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleInherit,    important = True  } ])
  -- Testing for parsing of bad css: typo in property name.
  , ( "order-bottom-style: inherit !important",  [])

  , ( "border-left-style: none",                 [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleNone,         important = False } ])
  , ( "border-left-style: hidden !important",    [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleHidden,       important = True  } ])
  , ( "border-left-style: dotted",               [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleDotted,       important = False } ])
  , ( "border-left-style: dashed !important",    [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleDashed,       important = True  } ])
  , ( "border-left-style: solid",                [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleSolid,        important = False } ])
  , ( "border-left-style: double !important",    [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleDouble,       important = True  } ])
  , ( "border-left-style: groove",               [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleGroove,       important = False } ])
  , ( "border-left-style: ridge !important",     [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleRidge,        important = True  } ])
  , ( "border-left-style: inset",                [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleInset,        important = False } ])
  , ( "border-left-style: outset !important",    [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleOutset,       important = True  } ])
  , ( "border-left-style: inherit",              [CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleInherit,      important = False } ])
  -- Testing for parsing of bad css: invalid value name.
  , ( "border-left-style: inheri !important",     [])




  , ( "border-top-width: inherit",                        [CssDeclWrapper { property = CssDeclarationBorderTopWidth CssValueBorderWidthInherit,                                            important = False } ])
  , ( "border-top-width: 1.0px",                          [CssDeclWrapper { property = CssDeclarationBorderTopWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 1.0))),     important = False } ])
  , ( "border-top-width: 2.0mm !important",               [CssDeclWrapper { property = CssDeclarationBorderTopWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsMm 2.0))),     important = True  } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-top-width: I.0px",                          [])

  , ( "border-right-width: inherit",                      [CssDeclWrapper { property = CssDeclarationBorderRightWidth CssValueBorderWidthInherit,                                          important = False } ])
  , ( "border-right-width: 1.5px !important",             [CssDeclWrapper { property = CssDeclarationBorderRightWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 1.5))),   important = True  } ])
  , ( "border-right-width: 2.0mm",                        [CssDeclWrapper { property = CssDeclarationBorderRightWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsMm 2.0))),   important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "border-rigth-width: 2.0mm",                        [])

  , ( "border-bottom-width: inherit !important",          [CssDeclWrapper { property = CssDeclarationBorderBottomWidth CssValueBorderWidthInherit,                                         important = True  } ])
  , ( "border-bottom-width: 1.0em",                       [CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceRelEm 1.0))),  important = False } ])
  , ( "border-bottom-width: 2.0ex !important",            [CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceRelEx 2.0))),  important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-bottom-width: 2.0ex !importan",             [CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceRelEx 2.0))),  important = False  } ])

  , ( "border-left-width: inherit",                       [CssDeclWrapper { property = CssDeclarationBorderLeftWidth CssValueBorderWidthInherit,                                           important = False } ])
  , ( "border-left-width: 1.0em",                         [CssDeclWrapper { property = CssDeclarationBorderLeftWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceRelEm 1.0))),    important = False } ])
  , ( "border-left-width: 2.0ex !important",              [CssDeclWrapper { property = CssDeclarationBorderLeftWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceRelEx 2.0))),    important = True  } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-left-width: anherit",                       [])




  , ( "color: inherit",                          [CssDeclWrapper { property = CssDeclarationColor CssValueColorInherit,        important = False } ])
  , ( "color: inherit !important",               [CssDeclWrapper { property = CssDeclarationColor CssValueColorInherit,        important = True  } ])
  , ( "color: red",                              [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0xff0000),    important = False } ])
  , ( "color: lime !important",                  [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0x00ff00),    important = True  } ]) -- Yes, "lime" not "green".
  , ( "color: blue !important;",                 [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0x0000ff),    important = True  } ])
  , ( "color: #abcdef;",                         [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0xabcdef),    important = False } ])
  ]




-- Shorthands. For now it's just most basic set of tests.
--
-- I'm putting them in separate list just because the set will become larger,
-- and I don't want the main list to become veeeery long.
--
-- TODO: none of the test inputs is using "important" keywords. Check if the
-- keyword is allowed in such declarations, and implement if necessary.
--
-- TODO: it looks like CSS standard allows omitting selected values, but our
-- parser doesn't support that.
parseDeclarationShorthandTestData =
  [
    ( "border: 10px inset #00fff1",           [ CssDeclWrapper { property = CssDeclarationBorderTopWidth    (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 10.0))), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightWidth  (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 10.0))), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 10.0))), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderLeftWidth   (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 10.0))), important = False }

                                              , CssDeclWrapper { property = CssDeclarationBorderTopStyle    CssValueBorderStyleInset, important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightStyle  CssValueBorderStyleInset, important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleInset, important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderLeftStyle   CssValueBorderStyleInset, important = False }

                                              , CssDeclWrapper { property = CssDeclarationBorderTopColor    $ CssValueBorderColor 0x00fff1 , important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightColor  $ CssValueBorderColor 0x00fff1 , important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0x00fff1 , important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderLeftColor   $ CssValueBorderColor 0x00fff1 , important = False }
                                              ])

  , ( "border-top: 5mm solid red",            [ CssDeclWrapper { property = CssDeclarationBorderTopWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsMm 5))), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleSolid,                                        important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderTopColor $ CssValueBorderColor 0xff0000,                                  important = False }
                                              ])

  , ( "border-right: 2mm dotted orange",      [ CssDeclWrapper { property = CssDeclarationBorderRightWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsMm 2))), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleDotted,                                       important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightColor $ CssValueBorderColor 0xffa500,                                  important = False }
                                              ])

  , ( "border-bottom: 17px inherit rgb(255, 0, 255)",    [ CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 17))), important = False }
                                                         , CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleInherit,                                       important = False }
                                                         , CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0xff00ff,                                   important = False }
                                                         ])

  , ( "border-left: 20em none inherit",      [ CssDeclWrapper { property = CssDeclarationBorderLeftWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceRelEm 20))), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleNone,                                          important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftColor $ CssValueBorderColorInherit,                                     important = False }
                                             ])

  , ( "border-width: 99px",                  [ CssDeclWrapper { property = CssDeclarationBorderTopWidth    (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 99))), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderRightWidth  (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 99))), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 99))), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftWidth   (CssValueBorderWidth (CssValueTypeLength (CssDistanceAbsPx 99))), important = False }
                                             ])

  , ( "border-style: double",               [ CssDeclWrapper { property = CssDeclarationBorderTopStyle    CssValueBorderStyleDouble,                                       important = False }
                                            , CssDeclWrapper { property = CssDeclarationBorderRightStyle  CssValueBorderStyleDouble,                                       important = False }
                                            , CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleDouble,                                       important = False }
                                            , CssDeclWrapper { property = CssDeclarationBorderLeftStyle   CssValueBorderStyleDouble,                                       important = False }
                                            ])

  , ( "border-color: rgb(0, 0, 255)",        [ CssDeclWrapper { property = CssDeclarationBorderTopColor    $ CssValueBorderColor 0x0000ff,                                   important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderRightColor  $ CssValueBorderColor 0x0000ff,                                   important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0x0000ff,                                   important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftColor   $ CssValueBorderColor 0x0000ff,                                   important = False }
                                             ])
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




testCases =
  [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
    TestCase (do
                 assertEqual "manual tests of standard  declarations with parseDeclaration" "" (parseDeclarationTest parseDeclarationTestData))
  , TestCase (do
                 assertEqual "manual tests of shorthand declarations with parseDeclaration" "" (parseDeclarationTest parseDeclarationShorthandTestData))
  ]




testsCssParser :: IO String
testsCssParser = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Tests.Css.Parser failed"

