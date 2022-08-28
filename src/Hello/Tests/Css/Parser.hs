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
    ( "background-attachment: scroll",              [CssDeclWrapper { property = CssDeclarationBackgroundAttachment CssValueBackgroundAttachmentScroll,       important = False } ])
  , ( "background-attachment: scroll !important",   [CssDeclWrapper { property = CssDeclarationBackgroundAttachment CssValueBackgroundAttachmentScroll,       important = True  } ])
  , ( "background-attachment: fixed",               [CssDeclWrapper { property = CssDeclarationBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = False } ])
  , ( "background-attachment: fixed !important",    [CssDeclWrapper { property = CssDeclarationBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "background-atachment: scroll",               [])
  -- Testing for parsing of bad css: invalid value.
  , ( "background-attachment: italic",              [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-attachment: fixed important",     [CssDeclWrapper { property = CssDeclarationBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = False  } ])




  , ( "background-color: inherit",                   [CssDeclWrapper { property = CssDeclarationBackgroundColor CssValueBackgroundColorInherit,             important = False } ])
  , ( "background-color: inherit !important",        [CssDeclWrapper { property = CssDeclarationBackgroundColor CssValueBackgroundColorInherit,             important = True  } ])
  , ( "background-color: blue",                      [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = False } ])
  , ( "background-color: blue !important",           [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } ])
  , ( "background-color: blue !important;",          [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } ])
  , ( "background-color: #00ff00;",                  [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = False } ])
  , ( "background-color: #00ff00 !important",        [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = True  } ])
  , ( "background-color: #00ff00 !important;",       [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = True  } ])
  , ( "background-color: rgb(0, 0, 255)",            [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = False } ])
  , ( "background-color: rgb(0, 0, 255) !important", [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "background-colo: blue",                       [])
  -- Testing for parsing of bad css: invalid value.
  , ( "background-color: square",                    [])
  , ( "background-color: 0x00ff00",                  []) -- Invalid format of HEX value
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-color: rgb(255, 0, 0) important",  [CssDeclWrapper { property = CssDeclarationBackgroundColor (CssValueBackgroundColorColor 0xff0000),        important = False  } ])




  -- TODO: decide what should be the value of CssValueBackgroundImageUri.
  -- Should it be just a verbatim stream, or some information about tokens that build the URI.
  -- TODO: write more tests
  , ( "background-image: url(\"background.png\")",     [CssDeclWrapper { property = CssDeclarationBackgroundImage (
                                                                           CssValueBackgroundImageUri "[CssTokStr \"background.png\",CssTokParenClose]"),         important = False } ])




  -- Support for background position in hello is almost non-existent, so this
  -- test set is very, very, very basic.
  , ( "background-position: left top",              [CssDeclWrapper { property = CssDeclarationBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = False } ])
  , ( "background-position: left top !important",   [CssDeclWrapper { property = CssDeclarationBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "backgroundposition: left top",               [])
  -- Testing for parsing of bad css: invalid value.
  --, ( "background-position: italic",                [])
  --, ( "background-position: left-top",              [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-position: left top important",    [CssDeclWrapper { property = CssDeclarationBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = False  } ])




  , ( "background-repeat: repeat",                  [CssDeclWrapper { property = CssDeclarationBackgroundRepeat CssValueBackgroundRepeatRepeat,         important = False } ])
  , ( "background-repeat: repeat-x !important",     [CssDeclWrapper { property = CssDeclarationBackgroundRepeat CssValueBackgroundRepeatRepeatX,        important = True  } ])
  , ( "background-repeat: repeat-y",                [CssDeclWrapper { property = CssDeclarationBackgroundRepeat CssValueBackgroundRepeatRepeatY,        important = False } ])
  , ( "background-repeat: no-repeat !important",    [CssDeclWrapper { property = CssDeclarationBackgroundRepeat CssValueBackgroundRepeatNoRepeat,       important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "background_repeat: repeat",                  [])
  -- Testing for parsing of bad css: invalid value.
  , ( "background-repeat: #00ff00",                 [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-repeat: no-repeat !importan",     [CssDeclWrapper { property = CssDeclarationBackgroundRepeat CssValueBackgroundRepeatNoRepeat,       important = False  } ])




  , ( "border-collapse: separate",               [CssDeclWrapper { property = CssDeclarationBorderCollapse CssValueBorderCollapseSeparate,        important = False } ])
  , ( "border-collapse: separate !important",    [CssDeclWrapper { property = CssDeclarationBorderCollapse CssValueBorderCollapseSeparate,        important = True  } ])
  , ( "border-collapse: collapse",               [CssDeclWrapper { property = CssDeclarationBorderCollapse CssValueBorderCollapseCollapse,        important = False } ])
  , ( "border-collapse: collapse !important",    [CssDeclWrapper { property = CssDeclarationBorderCollapse CssValueBorderCollapseCollapse,        important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "border-colapse: block",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-collapse: #00ff00",                [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-collapse: separate !importan",     [CssDeclWrapper { property = CssDeclarationBorderCollapse CssValueBorderCollapseSeparate,        important = False  } ])




  , ( "border-spacing:  1.5px !important",            [CssDeclWrapper { property = CssDeclarationBorderSpacing (CssValueBorderSpacingDistance (CssDistanceAbsPx  1.5)),  important = True  } ])
  , ( "border-spacing:  2.0mm",                       [CssDeclWrapper { property = CssDeclarationBorderSpacing (CssValueBorderSpacingDistance (CssDistanceAbsMm  2.0)),  important = False } ])
  , ( "border-spacing: 13.5em",                       [CssDeclWrapper { property = CssDeclarationBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEm 13.5)),  important = False } ])
  , ( "border-spacing: 44.0ex !important",            [CssDeclWrapper { property = CssDeclarationBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEx 44.0)),  important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "border_spacing: 52.0mm",                       [])
  -- Testing for parsing of bad css: invalid property value.
  , ( "border-spacing: latin",                        [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-spacing: 74.0ex !importan",             [CssDeclWrapper { property = CssDeclarationBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEx 74.0)),  important = False  } ])




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




  , ( "border-top-width: inherit",                        [CssDeclWrapper { property = CssDeclarationBorderTopWidth CssValueBorderWidthInherit,                               important = False } ])
  , ( "border-top-width: 1.0px",                          [CssDeclWrapper { property = CssDeclarationBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)),     important = False } ])
  , ( "border-top-width: 2.0mm !important",               [CssDeclWrapper { property = CssDeclarationBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2.0)),     important = True  } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-top-width: I.0px",                          [])

  , ( "border-right-width: inherit",                      [CssDeclWrapper { property = CssDeclarationBorderRightWidth CssValueBorderWidthInherit,                             important = False } ])
  , ( "border-right-width: 1.5px !important",             [CssDeclWrapper { property = CssDeclarationBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.5)),   important = True  } ])
  , ( "border-right-width: 2.0mm",                        [CssDeclWrapper { property = CssDeclarationBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2.0)),   important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "border-rigth-width: 2.0mm",                        [])

  , ( "border-bottom-width: inherit !important",          [CssDeclWrapper { property = CssDeclarationBorderBottomWidth CssValueBorderWidthInherit,                            important = True  } ])
  , ( "border-bottom-width: 1.0em",                       [CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEm 1.0)),  important = False } ])
  , ( "border-bottom-width: 2.0ex !important",            [CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),  important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-bottom-width: 2.0ex !importan",             [CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),  important = False  } ])

  , ( "border-left-width: inherit",                       [CssDeclWrapper { property = CssDeclarationBorderLeftWidth CssValueBorderWidthInherit,                              important = False } ])
  , ( "border-left-width: 1.0em",                         [CssDeclWrapper { property = CssDeclarationBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEm 1.0)),    important = False } ])
  , ( "border-left-width: 2.0ex !important",              [CssDeclWrapper { property = CssDeclarationBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),    important = True  } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-left-width: anherit",                       [])




  , ( "color: inherit",                          [CssDeclWrapper { property = CssDeclarationColor CssValueColorInherit,        important = False } ])
  , ( "color: inherit !important",               [CssDeclWrapper { property = CssDeclarationColor CssValueColorInherit,        important = True  } ])
  , ( "color: red",                              [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0xff0000),    important = False } ])
  , ( "color: lime !important",                  [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0x00ff00),    important = True  } ]) -- Yes, "lime" not "green".
  , ( "color: blue !important;",                 [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0x0000ff),    important = True  } ])
  , ( "color: #abcdef;",                         [CssDeclWrapper { property = CssDeclarationColor (CssValueColor 0xabcdef),    important = False } ])




    -- For now only quoted strings are supported (with single or double quotes).
  , ( "content: \"\"",                           [CssDeclWrapper { property = CssDeclarationContent (CssValueContent ""),        important = False } ])
  , ( "content: \"\" !important",                [CssDeclWrapper { property = CssDeclarationContent (CssValueContent ""),        important = True } ])
  , ( "content: \"bullet\"",                     [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "bullet"),  important = False } ])
  , ( "content: \"bullet\" !important",          [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "bullet"),  important = True } ])
  , ( "content: \"train\"",                      [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "train"),   important = False } ])
  , ( "content: \"train\" !important",           [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "train"),   important = True } ])
  , ( "content: ''",                             [CssDeclWrapper { property = CssDeclarationContent (CssValueContent ""),        important = False } ])
  , ( "content: '' !important",                  [CssDeclWrapper { property = CssDeclarationContent (CssValueContent ""),        important = True } ])
  , ( "content: 'bus'",                          [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "bus"),     important = False } ])
  , ( "content: 'bus' !important",               [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "bus"),     important = True } ])
  , ( "content: 'car'",                          [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "car"),     important = False } ])
  , ( "content: 'car' !important",               [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "car"),     important = True } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "contet: \"bullet\"",                      [])
    -- Testing for parsing of bad css: invalid value.
  , ( "content: train",                          [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "content: \"bullet\" !improtant",          [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "bullet"),  important = False  } ])
  , ( "content: 'train' !improtant",             [CssDeclWrapper { property = CssDeclarationContent (CssValueContent "train"),   important = False  } ])




  , ( "cursor: crosshair",            [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorCrosshair,   important = False } ])
  , ( "cursor: default !important",   [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorDefault,     important = True  } ])
  , ( "cursor: pointer",              [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorPointer,     important = False } ])
  , ( "cursor: move !important",      [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorMove,        important = True  } ])
  , ( "cursor: e-resize",             [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorEResize,     important = False } ])
  , ( "cursor: ne-resize !important", [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorNeResize,    important = True  } ])
  , ( "cursor: nw-resize",            [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorNwResize,    important = False } ])
  , ( "cursor: n-resize !important",  [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorNResize,     important = True  } ])
  , ( "cursor: se-resize",            [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorSeResize,    important = False } ])
  , ( "cursor: sw-resize !important", [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorSwResize,    important = True  } ])
  , ( "cursor: s-resize",             [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorSResize,     important = False } ])
  , ( "cursor: w-resize !important",  [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorWResize,     important = True  } ])
  , ( "cursor: text",                 [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorText,        important = False } ])
  , ( "cursor: wait !important",      [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorWait,        important = True  } ])
  , ( "cursor: help",                 [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorHelp,        important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "cursr: crosshair",             [])
  -- Testing for parsing of bad css: invalid value.
  , ( "cursor: ponter",               [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "cursor: help !improtant",      [CssDeclWrapper { property = CssDeclarationCursor CssValueCursorHelp,        important = False  } ])





  , ( "display: block",                         [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayBlock,              important = False } ])
  , ( "display: inline !important",             [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayInline,             important = True  } ])
  , ( "display: inline-block",                  [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayInlineBlock,        important = False } ])
  , ( "display: list-item !important",          [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayListItem,           important = True  } ])
  , ( "display: none",                          [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayNone,               important = False } ])
  , ( "display: table !important",              [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTable,              important = True  } ])
  , ( "display: table-row-group",               [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTableRowGroup,      important = False } ])
  , ( "display: table-header-group !important", [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTableHeaderGroup,   important = True  } ])
  , ( "display: table-footer-group",            [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTableFooterGroup,   important = False } ])
  , ( "display: table-row !important",          [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTableRow,           important = True  } ])
  , ( "display: table-cell",                    [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTableCell,          important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "dsiplay: block",                         [])
  -- Testing for parsing of bad css: invalid value.
  , ( "display: rgb(0, 100, 200)",              [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "display: table !improtant",              [CssDeclWrapper { property = CssDeclarationDisplay CssValueDisplayTable,              important = False  } ])




    -- TODO: "!important" keyword is not parsed correctly, fix it.
    -- TODO: rules for font-family are compilcated, but we don't support them well, fix it.
  , ("font-family: monospace",                  [CssDeclWrapper { property = CssDeclarationFontFamily $ CssValueFontFamilyList ["monospace"],             important = False } ])
  , ("font-family: \"Comic Sans\", serif",      [CssDeclWrapper { property = CssDeclarationFontFamily $ CssValueFontFamilyList ["Comic Sans", "serif"],   important = False } ])
  , ("font-family: 'My Font', cursive",         [CssDeclWrapper { property = CssDeclarationFontFamily $ CssValueFontFamilyList ["My Font", "cursive"],    important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "foht-family: monospace",                 [])




  , ("font-size: xx-small",            [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeXXSmall,   important = False } ])
  , ("font-size: x-small !important",  [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeXSmall,    important = True  } ])
  , ("font-size: small",               [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeSmall,     important = False } ])
  , ("font-size: medium !important",   [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeMedium,    important = True  } ])
  , ("font-size: large",               [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeLarge,     important = False } ])
  , ("font-size: x-large !important",  [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeXLarge,    important = True  } ])
  , ("font-size: xx-large",            [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeXXLarge,   important = False } ])
  , ("font-size: larger !important",   [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeLarger,    important = True  } ])
  , ("font-size: smaller",             [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeSmaller,   important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "font-site: small",              [])
  -- Testing for parsing of bad css: invalid value.
  , ( "font-size: square",             [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-size: large important",    [CssDeclWrapper { property = CssDeclarationFontSize CssValueFontSizeLarge,     important = False  } ])




  , ("font-style: normal !important",    [CssDeclWrapper { property = CssDeclarationFontStyle CssValueFontStyleNormal,   important = True  } ])
  , ("font-style: italic",               [CssDeclWrapper { property = CssDeclarationFontStyle CssValueFontStyleItalic,   important = False } ])
  , ("font-style: oblique !important",   [CssDeclWrapper { property = CssDeclarationFontStyle CssValueFontStyleOblique,  important = True  } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "font-syle: normal",               [])
  -- Testing for parsing of bad css: invalid value.
  , ( "font-style: obligue",             [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-style: normal !!important",  [CssDeclWrapper { property = CssDeclarationFontStyle CssValueFontStyleNormal,   important = False  } ])




  , ("font-variant: normal !important",    [CssDeclWrapper { property = CssDeclarationFontVariant CssValueFontVariantNormal,      important = True  } ])
  , ("font-variant: small-caps",           [CssDeclWrapper { property = CssDeclarationFontVariant CssValueFontVariantSmallCaps,   important = False } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "font-wariant: normal",              [])
  -- Testing for parsing of bad css: invalid value.
  , ( "font-variant: xx-large",            [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-variant: normal !_mportant",   [CssDeclWrapper { property = CssDeclarationFontVariant CssValueFontVariantNormal,      important = False  } ])




  , ("font-weight: normal",             [CssDeclWrapper { property = CssDeclarationFontWeight CssValueFontWeightNormal,     important = False } ])
  , ("font-weight: bold !important",    [CssDeclWrapper { property = CssDeclarationFontWeight CssValueFontWeightBold,       important = True  } ])
  , ("font-weight: bolder",             [CssDeclWrapper { property = CssDeclarationFontWeight CssValueFontWeightBolder,     important = False } ])
  , ("font-weight: lighter !important", [CssDeclWrapper { property = CssDeclarationFontWeight CssValueFontWeightLighter,    important = True  } ])
  , ("font-weight: 100 !important",     [CssDeclWrapper { property = CssDeclarationFontWeight $ CssValueFontWeightInt 100,  important = True  } ])
  , ("font-weight: 900",                [CssDeclWrapper { property = CssDeclarationFontWeight $ CssValueFontWeightInt 900,  important = False } ])

    -- Testing for parsing of bad css: invalid property name.
  , ("font-weigth: bold",               [])
    -- Testing for parsing of bad css: invalid value.
  , ("font-weight: light",              [])
  , ("font-weight: 1200",               [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-weight: normal !_mportant", [CssDeclWrapper { property = CssDeclarationFontWeight CssValueFontWeightNormal,   important = False  } ])




  , ("height: auto",                     [CssDeclWrapper { property = CssDeclarationHeight . CssValueHeightDistance $ CssDistanceAuto,                    important = False } ])
  , ("height: auto !important",          [CssDeclWrapper { property = CssDeclarationHeight . CssValueHeightDistance $ CssDistanceAuto,                    important = True  } ])
  , ("height:   1px",                    [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceAbsPx   1.0)),             important = False } ])
  , ("height:   1px !important",         [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceAbsPx   1.0)),             important = True  } ])
  , ("height:  22.22mm",                 [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = False } ])
  , ("height:  22.22mm !important",      [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = True  } ])
  , ("height:  33.3em",                  [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceRelEm  33.3)),             important = False } ])
  , ("height:  33.3em !important",       [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceRelEm  33.3)),             important = True  } ])
  , ("height: 444.44ex",                 [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceRelEx 444.44)),            important = False } ])
  , ("height: 444.44ex !important",      [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceRelEx 444.44)),            important = True  } ])

    -- Testing for parsing of bad css: invalid property name.
  , ("heigth:  77.7em",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ("height:  left",                    [])
    -- TODO: per CSS2.2 negative values are invalid. Fix this case in parser.
  , ("height: -500.0mm",                 [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceAbsMm (-500.00))),         important = False } ])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("height:  22.22mm !importat",       [CssDeclWrapper { property = CssDeclarationHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = False } ])




  , ("letter-spacing: normal",             [CssDeclWrapper { property = CssDeclarationLetterSpacing CssValueLetterSpacingNormal,                                 important = False } ])
  , ("letter-spacing: normal !important",  [CssDeclWrapper { property = CssDeclarationLetterSpacing CssValueLetterSpacingNormal,                                 important = True  } ])
  , ("letter-spacing: 10px",               [CssDeclWrapper { property = CssDeclarationLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx 10.0)),     important = False } ])
  , ("letter-spacing: 10px !important",    [CssDeclWrapper { property = CssDeclarationLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx 10.0)),     important = True  } ])
  , ("letter-spacing: -10px",              [CssDeclWrapper { property = CssDeclarationLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx (-10.0))),  important = False } ])
  , ("letter-spacing: -10px !important",   [CssDeclWrapper { property = CssDeclarationLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx (-10.0))),  important = True  } ])
  , ("letter-spacing: 5em",                [CssDeclWrapper { property = CssDeclarationLetterSpacing (CssValueLetterSpacingDistance (CssDistanceRelEm 5.0)),      important = False } ])
  , ("letter-spacing: 5em !important",     [CssDeclWrapper { property = CssDeclarationLetterSpacing (CssValueLetterSpacingDistance (CssDistanceRelEm 5.0)),      important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ("leter-spacing: normal",              [])
  -- Testing for parsing of bad css: invalid value.
  , ("letter-spacing: bold",               [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("letter-spacing: normal !_omportant",  [CssDeclWrapper { property = CssDeclarationLetterSpacing CssValueLetterSpacingNormal,                              important = False  } ])




  , ("line-height: normal",             [CssDeclWrapper { property = CssDeclarationLineHeight CssValueLineHeightNormal,                                 important = False } ])
  , ("line-height: normal !important",  [CssDeclWrapper { property = CssDeclarationLineHeight CssValueLineHeightNormal,                                 important = True  } ])
  , ("line-height: 10px",               [CssDeclWrapper { property = CssDeclarationLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx 10.0)),     important = False } ])
  , ("line-height: 10px !important",    [CssDeclWrapper { property = CssDeclarationLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx 10.0)),     important = True  } ])
  , ("line-height: -10px",              [CssDeclWrapper { property = CssDeclarationLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx (-10.0))),  important = False } ])
  , ("line-height: -10px !important",   [CssDeclWrapper { property = CssDeclarationLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx (-10.0))),  important = True  } ])
  , ("line-height: 5em",                [CssDeclWrapper { property = CssDeclarationLineHeight (CssValueLineHeightDistance (CssDistanceRelEm 5.0)),      important = False } ])
  , ("line-height: 5em !important",     [CssDeclWrapper { property = CssDeclarationLineHeight (CssValueLineHeightDistance (CssDistanceRelEm 5.0)),      important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ("line-heigth: normal",             [])
  -- Testing for parsing of bad css: invalid value.
  , ("line-height: bold",               [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("line-height: normal important",  [CssDeclWrapper { property = CssDeclarationLineHeight CssValueLineHeightNormal,                                important = False  } ])




  , ( "list-style-position: inside",                    [CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionInside,   important = False } ])
  , ( "list-style-position: inside !important",         [CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionInside,   important = True } ])
  , ( "list-style-position: outside",                   [CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionOutside,  important = False } ])
  , ( "list-style-position: outside !important",        [CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionOutside,  important = True } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "list-style-position: outide !important",         [])




  , ( "list-style-type: disc !important",                 [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeDisc,                important = True  } ])
  , ( "list-style-type: circle",                          [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeCircle,              important = False } ])
  , ( "list-style-type: square !important",               [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeSquare,              important = True  } ])
  , ( "list-style-type: decimal",                         [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeDecimal,             important = False } ])
  , ( "list-style-type: decimal-leading-zero !important", [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeDecimalLeadingZero,  important = True  } ])
  , ( "list-style-type: lower-roman",                     [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeLowerRoman,          important = False } ])
  , ( "list-style-type: upper-roman !important",          [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeUpperRoman,          important = True  } ])
  , ( "list-style-type: lower-greek",                     [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeLowerGreek,          important = False } ])
  , ( "list-style-type: lower-alpha !important",          [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeLowerAlpha,          important = True  } ])
  , ( "list-style-type: lower-latin",                     [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeLowerLatin,          important = False } ])
  , ( "list-style-type: upper-alpha !important",          [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeUpperAlpha,          important = True  } ])
  , ( "list-style-type: upper-latin",                     [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeUpperLatin,          important = False } ])
  , ( "list-style-type: hebrew !important",               [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeHebrew,              important = True  } ])
  , ( "list-style-type: armenian",                        [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeArmenian,            important = False } ])
  , ( "list-style-type: georgian !important",             [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeGeorgian,            important = True  } ])
  , ( "list-style-type: cjk-ideographic",                 [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeCjkIdeographic,      important = False } ])
  , ( "list-style-type: hiragana !important",             [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeHiragana,            important = True  } ])
  , ( "list-style-type: katakana",                        [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeKatakana,            important = False } ])
  , ( "list-style-type: hiragana-iroha !important",       [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeHiraganaIroha,       important = True  } ])
  , ( "list-style-type: katakana-iroha",                  [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeKatakanaIroha,       important = False } ])
  , ( "list-style-type: none !important",                 [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeNone,                important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "list-styletype: upper-latin",                      [])
  -- Testing for parsing of bad css: invalid value.
  , ( "list-style-type: lower-ronan",                     [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "list-style-type: none !improtant",                 [CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeNone,                important = False  } ])




  , ( "margin-top: auto",                      [CssDeclWrapper { property = CssDeclarationMarginTop . CssValueMarginDistance $ CssDistanceAuto,              important = False } ])
  , ( "margin-top: auto !important",           [CssDeclWrapper { property = CssDeclarationMarginTop . CssValueMarginDistance $ CssDistanceAuto,              important = True  } ])
  , ( "margin-top:  1px",                      [CssDeclWrapper { property = CssDeclarationMarginTop (CssValueMarginDistance (CssDistanceAbsPx  1.0)),        important = False } ])
  , ( "margin-top:  2.2mm !important",         [CssDeclWrapper { property = CssDeclarationMarginTop (CssValueMarginDistance (CssDistanceAbsMm  2.2)),        important = True  } ])
  , ( "margin-top:  3.0em",                    [CssDeclWrapper { property = CssDeclarationMarginTop (CssValueMarginDistance (CssDistanceRelEm  3.0)),        important = False } ])
  , ( "margin-top: 93.0ex",                    [CssDeclWrapper { property = CssDeclarationMarginTop (CssValueMarginDistance (CssDistanceRelEx 93.0)),        important = False } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-to: 11.0px",                     [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-top: red",                       [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-top: 26.6px !inportant",         [CssDeclWrapper { property = CssDeclarationMarginTop (CssValueMarginDistance (CssDistanceAbsPx 26.6)),        important = False  } ])




  , ( "margin-right: auto",                    [CssDeclWrapper { property = CssDeclarationMarginRight . CssValueMarginDistance $ CssDistanceAuto,            important = False } ])
  , ( "margin-right: auto !important",         [CssDeclWrapper { property = CssDeclarationMarginRight . CssValueMarginDistance $ CssDistanceAuto,            important = True  } ])
  , ( "margin-right: 111px",                   [CssDeclWrapper { property = CssDeclarationMarginRight (CssValueMarginDistance (CssDistanceAbsPx 111.0)),     important = False } ])
  , ( "margin-right: 222mm !important",        [CssDeclWrapper { property = CssDeclarationMarginRight (CssValueMarginDistance (CssDistanceAbsMm 222.0)),     important = True  } ])
  , ( "margin-right: 333.0em",                 [CssDeclWrapper { property = CssDeclarationMarginRight (CssValueMarginDistance (CssDistanceRelEm 333.0)),     important = False } ])
  , ( "margin-right: 444.0ex !important",      [CssDeclWrapper { property = CssDeclarationMarginRight (CssValueMarginDistance (CssDistanceRelEx 444.0)),     important = True  } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-rigth: 11.0px",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-right: italic",                  [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-right: 33.6px !inportant",       [CssDeclWrapper { property = CssDeclarationMarginRight (CssValueMarginDistance (CssDistanceAbsPx 33.6)),      important = False  } ])




  , ( "margin-bottom: auto",                   [CssDeclWrapper { property = CssDeclarationMarginBottom . CssValueMarginDistance $ CssDistanceAuto,           important = False } ])
  , ( "margin-bottom: auto !important",        [CssDeclWrapper { property = CssDeclarationMarginBottom . CssValueMarginDistance $ CssDistanceAuto,           important = True  } ])
  , ( "margin-bottom: 1.110px",                [CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceAbsPx 1.11)),     important = False } ])
  , ( "margin-bottom: 2.220mm !important",     [CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceAbsMm 2.22)),     important = True  } ])
  , ( "margin-bottom: 3.330em",                [CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceRelEm 3.33)),     important = False } ])
  , ( "margin-bottom: 4.440ex !important",     [CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceRelEx 4.44)),     important = True  } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-botom: 11.0px",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-bottom: none",                   [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-bottom: 33.6px !inportant",      [CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceAbsPx 33.6)),     important = False  } ])




  , ( "margin-left: auto",                     [CssDeclWrapper { property = CssDeclarationMarginLeft . CssValueMarginDistance $ CssDistanceAuto,             important = False } ])
  , ( "margin-left: auto !important",          [CssDeclWrapper { property = CssDeclarationMarginLeft . CssValueMarginDistance $ CssDistanceAuto,             important = True  } ])
  , ( "margin-left: 1.110px !important",       [CssDeclWrapper { property = CssDeclarationMarginLeft (CssValueMarginDistance (CssDistanceAbsPx 1.11)),       important = True  } ])
  , ( "margin-left: 2.220mm",                  [CssDeclWrapper { property = CssDeclarationMarginLeft (CssValueMarginDistance (CssDistanceAbsMm 2.22)),       important = False } ])
  , ( "margin-left: 3.330em !important",       [CssDeclWrapper { property = CssDeclarationMarginLeft (CssValueMarginDistance (CssDistanceRelEm 3.33)),       important = True  } ])
  , ( "margin-left: 4.440ex",                  [CssDeclWrapper { property = CssDeclarationMarginLeft (CssValueMarginDistance (CssDistanceRelEx 4.44)),       important = False } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin_left: 11.0px",                   [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-left: latin",                    [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-left: 33.6px !inportant",        [CssDeclWrapper { property = CssDeclarationMarginLeft (CssValueMarginDistance (CssDistanceAbsPx 33.6)),       important = False  } ])




  , ( "padding-top: 1.0px",                   [CssDeclWrapper { property = CssDeclarationPaddingTop (CssValuePadding (CssDistanceAbsPx 1.0)),       important = False } ])
  , ( "padding-top: 2.3mm !important",        [CssDeclWrapper { property = CssDeclarationPaddingTop (CssValuePadding (CssDistanceAbsMm 2.3)),       important = True  } ])
  , ( "padding-top: 4.5em",                   [CssDeclWrapper { property = CssDeclarationPaddingTop (CssValuePadding (CssDistanceRelEm 4.5)),       important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-to: 1.0px",                    [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-top: red",                     [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-top: 6.6px !inportant",        [CssDeclWrapper { property = CssDeclarationPaddingTop (CssValuePadding (CssDistanceAbsPx 6.6)),       important = False  } ])




  , ( "padding-right: 1.0px",                 [CssDeclWrapper { property = CssDeclarationPaddingRight (CssValuePadding (CssDistanceAbsPx 1.0)),     important = False } ])
  , ( "padding-right: 2.3mm !important",      [CssDeclWrapper { property = CssDeclarationPaddingRight (CssValuePadding (CssDistanceAbsMm 2.3)),     important = True  } ])
  , ( "padding-right: 4.5em",                 [CssDeclWrapper { property = CssDeclarationPaddingRight (CssValuePadding (CssDistanceRelEm 4.5)),     important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-right: red",                   [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-right: 6.6px !inportant",      [CssDeclWrapper { property = CssDeclarationPaddingRight (CssValuePadding (CssDistanceAbsPx 6.6)),     important = False  } ])




  , ( "padding-bottom: 1.0px",                [CssDeclWrapper { property = CssDeclarationPaddingBottom (CssValuePadding (CssDistanceAbsPx 1.0)),    important = False } ])
  , ( "padding-bottom: 2.3mm !important",     [CssDeclWrapper { property = CssDeclarationPaddingBottom (CssValuePadding (CssDistanceAbsMm 2.3)),    important = True  } ])
  , ( "padding-bottom: 4.5em",                [CssDeclWrapper { property = CssDeclarationPaddingBottom (CssValuePadding (CssDistanceRelEm 4.5)),    important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-bottom: red",                  [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-bottom: 6.6px !inportant",     [CssDeclWrapper { property = CssDeclarationPaddingBottom (CssValuePadding (CssDistanceAbsPx 6.6)),    important = False  } ])




  , ( "padding-left: 1.0px",                  [CssDeclWrapper { property = CssDeclarationPaddingLeft (CssValuePadding (CssDistanceAbsPx 1.0)),      important = False } ])
  , ( "padding-left: 2.3mm !important",       [CssDeclWrapper { property = CssDeclarationPaddingLeft (CssValuePadding (CssDistanceAbsMm 2.3)),      important = True  } ])
  , ( "padding-left: 4.5em",                  [CssDeclWrapper { property = CssDeclarationPaddingLeft (CssValuePadding (CssDistanceRelEm 4.5)),      important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-left: red",                    [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-left: 6.6px !inportant",       [CssDeclWrapper { property = CssDeclarationPaddingLeft (CssValuePadding (CssDistanceAbsPx 6.6)),      important = False  } ])




  , ( "text-indent:  1.1px !important",            [CssDeclWrapper { property = CssDeclarationTextIndent (CssValueTextIndentDistance (CssDistanceAbsPx     1.1)),   important = True  } ])
  , ( "text-indent:  2.2mm",                       [CssDeclWrapper { property = CssDeclarationTextIndent (CssValueTextIndentDistance (CssDistanceAbsMm     2.2)),   important = False } ])
  , ( "text-indent: 13.3em",                       [CssDeclWrapper { property = CssDeclarationTextIndent (CssValueTextIndentDistance (CssDistanceRelEm    13.3)),   important = False } ])
  , ( "text-indent: 44.4ex !important",            [CssDeclWrapper { property = CssDeclarationTextIndent (CssValueTextIndentDistance (CssDistanceRelEx    44.4)),   important = True  } ])
  -- From a real web page :)
  , ( "text-indent: -700em",                       [CssDeclWrapper { property = CssDeclarationTextIndent (CssValueTextIndentDistance (CssDistanceRelEm (-700.0))),  important = False } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "test-indent: 55.5mm",                       [])
  -- Testing for parsing of bad css: invalid property value.
  , ( "text-indent: justify",                      [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "text-indent: 77.7ex !importan",             [CssDeclWrapper { property = CssDeclarationTextIndent (CssValueTextIndentDistance (CssDistanceRelEx 77.7)),  important = False  } ])




  , ("text-align: left !important",       [CssDeclWrapper { property = CssDeclarationTextAlign CssValueTextAlignLeft,     important = True  } ])
  , ("text-align: right",                 [CssDeclWrapper { property = CssDeclarationTextAlign CssValueTextAlignRight,    important = False } ])
  , ("text-align: center !important",     [CssDeclWrapper { property = CssDeclarationTextAlign CssValueTextAlignCenter,   important = True  } ])
  , ("text-align: justify",               [CssDeclWrapper { property = CssDeclarationTextAlign CssValueTextAlignJustify,  important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ("test-align: left",                  [])
  -- Testing for parsing of bad css: invalid value.
  , ("text-align: italic",                [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("text-align: left !!important",      [CssDeclWrapper { property = CssDeclarationTextAlign CssValueTextAlignLeft,     important = False  } ])




  , ("text-transform: none",                  [CssDeclWrapper { property = CssDeclarationTextTransform CssValueTextTransformNone,        important = False } ])
  , ("text-transform: capitalize !important", [CssDeclWrapper { property = CssDeclarationTextTransform CssValueTextTransformCapitalize,  important = True  } ])
  , ("text-transform: uppercase",             [CssDeclWrapper { property = CssDeclarationTextTransform CssValueTextTransformUppercase,   important = False } ])
  , ("text-transform: lowercase !important",  [CssDeclWrapper { property = CssDeclarationTextTransform CssValueTextTransformLowercase,   important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ("test-transform: none",                  [])
  -- Testing for parsing of bad css: invalid value.
  , ("text-transform: 1.0px",                 [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("text-transform: uppercase _important",  [CssDeclWrapper { property = CssDeclarationTextTransform CssValueTextTransformUppercase,   important = False  } ])




  -- First some simple cases, where only one value appears in input.
  , ("text-decoration: underline !important",     [CssDeclWrapper { property = CssDeclarationTextDecoration [CssValueTextDecorationUnderline],    important = True  } ])
  , ("text-decoration: overline",                 [CssDeclWrapper { property = CssDeclarationTextDecoration [CssValueTextDecorationOverline],     important = False } ])
  , ("text-decoration: line-through !important",  [CssDeclWrapper { property = CssDeclarationTextDecoration [CssValueTextDecorationLineThrough],  important = True  } ])
  , ("text-decoration: blink",                    [CssDeclWrapper { property = CssDeclarationTextDecoration [CssValueTextDecorationBlink],        important = False } ])

  -- Now few valid values. Notice that in different test cases the values appear in in different order.
  , ("text-decoration: underline overline line-through blink", [CssDeclWrapper { property = CssDeclarationTextDecoration
                                                                                            [ CssValueTextDecorationUnderline
                                                                                            , CssValueTextDecorationOverline
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            , CssValueTextDecorationBlink
                                                                                            ],
                                                                                 important = False } ])
  , ("text-decoration: blink overline underline line-through", [CssDeclWrapper { property = CssDeclarationTextDecoration
                                                                                            [ CssValueTextDecorationBlink
                                                                                            , CssValueTextDecorationOverline
                                                                                            , CssValueTextDecorationUnderline
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            ],
                                                                                 important = False } ])
  , ("text-decoration: overline line-through",                 [CssDeclWrapper { property = CssDeclarationTextDecoration
                                                                                            [ CssValueTextDecorationOverline
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            ],
                                                                                 important = False } ])
  , ("text-decoration: blink line-through !important",         [CssDeclWrapper { property = CssDeclarationTextDecoration
                                                                                            [ CssValueTextDecorationBlink
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            ],
                                                                                 important = True } ])

  -- Testing for parsing of bad css: invalid property name.
  , ("test-decoration: overline",                     [])
  -- Testing for parsing of bad css: invalid value. Notice that a single invalid value token invalidates entire property.
  , ("text-decoration: blue",                         [])
  , ("text-decoration: underline blue",               [])
  , ("text-decoration: 1.0px overline",               [])
  , ("text-decoration: underline italic blink",       [])
  , ("text-decoration: underline overline brink",     [])
  -- Testing for parsing of bad css: misspelled "important" word.
  --
  -- Notice that in this case the "_important" word is treated as one of
  -- possible decorations. The word is not a valid decoration, so entrie
  -- declaration is rejected.
  , ("text-decoration: underline _important",         [])




  , ( "vertical-align: top !important",       [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignTop,        important = True  } ])
  , ( "vertical-align: bottom",               [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignBottom,     important = False } ])
  , ( "vertical-align: middle !important",    [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignMiddle,     important = True  } ])
  , ( "vertical-align: baseline",             [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignBaseline,   important = False } ])
  , ( "vertical-align: sub !important",       [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignSub,        important = True  } ])
  , ( "vertical-align: super",                [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignSuper,      important = False } ])
  , ( "vertical-align: text-top !important",  [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignTextTop,    important = True  } ])
  , ( "vertical-align: text-bottom",          [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignTextBottom, important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "ertical-align: pre",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "vertical-align: suber",                [])
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "vertical-align: top !!important",      [CssDeclWrapper { property = CssDeclarationVerticalAlign CssValueVerticalAlignTop,        important = False } ])




  , ("width: auto",                     [CssDeclWrapper { property = CssDeclarationWidth . CssValueWidthDistance $ CssDistanceAuto,                    important = False } ])
  , ("width: auto !important",          [CssDeclWrapper { property = CssDeclarationWidth . CssValueWidthDistance $ CssDistanceAuto,                    important = True  } ])
  , ("width:   1px",                    [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceAbsPx   1.0)),             important = False } ])
  , ("width:   1px !important",         [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceAbsPx   1.0)),             important = True  } ])
  , ("width:  22.22mm",                 [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = False } ])
  , ("width:  22.22mm !important",      [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = True  } ])
  , ("width:  33.3em",                  [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceRelEm  33.3)),             important = False } ])
  , ("width:  33.3em !important",       [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceRelEm  33.3)),             important = True  } ])
  , ("width: 444.44ex",                 [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceRelEx 444.44)),            important = False } ])
  , ("width: 444.44ex !important",      [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceRelEx 444.44)),            important = True  } ])

    -- Testing for parsing of bad css: invalid property name.
  , ("widht:  77.7em",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ("width:  left",                    [])
    -- TODO: per CSS2.2 negative values are invalid. Fix this case in parser.
  , ("width: -500.0mm",                 [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceAbsMm (-500.00))),         important = False } ])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("width:  22.22mm !importat",       [CssDeclWrapper { property = CssDeclarationWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = False } ])




  , ( "white-space: normal !important",     [CssDeclWrapper { property = CssDeclarationWhitespace CssValueWhitespaceNormal,   important = True  } ])
  , ( "white-space: pre",                   [CssDeclWrapper { property = CssDeclarationWhitespace CssValueWhitespacePre,      important = False } ])
  , ( "white-space: nowrap !important",     [CssDeclWrapper { property = CssDeclarationWhitespace CssValueWhitespaceNoWrap,   important = True  } ])
  , ( "white-space: pre-wrap",              [CssDeclWrapper { property = CssDeclarationWhitespace CssValueWhitespacePreWrap,  important = False } ])
  , ( "white-space: pre-line  !important",  [CssDeclWrapper { property = CssDeclarationWhitespace CssValueWhitespacePreLine,  important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "white-spac: pre",                    [])
  -- Testing for parsing of bad css: invalid value.
  , ( "white-space: prewrap",               [])
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "white-space: pre important",         [CssDeclWrapper { property = CssDeclarationWhitespace CssValueWhitespacePre,      important = False } ])




  , ( "word-spacing: normal !important",    [CssDeclWrapper { property = CssDeclarationWordSpacing CssValueWordSpacingNormal,                              important = True  } ])
  , ( "word-spacing: 1.0px",                [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacingDistance (CssDistanceAbsPx 1.0)),   important = False } ])
  , ( "word-spacing: 2.5mm !important",     [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacingDistance (CssDistanceAbsMm 2.5)),   important = True  } ])
  , ( "word-spacing: 3.6em",                [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacingDistance (CssDistanceRelEm 3.6)),   important = False } ])
  , ( "word-spacing: 4.7ex !important",     [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacingDistance (CssDistanceRelEx 4.7)),   important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "words-pacing: normal !important",    [])
  -- Testing for parsing of bad css: invalid value. TODO: shouldn't "1" be considered a valid value?
  , ( "word-spacing: 1;0xz",                [])
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "word-spacing: normal !importan",     [CssDeclWrapper { property = CssDeclarationWordSpacing CssValueWordSpacingNormal,   important = False  } ])
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
    ( "border: 10px inset #00fff1",           [ CssDeclWrapper { property = CssDeclarationBorderTopWidth    (CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightWidth  (CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderLeftWidth   (CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)), important = False }

                                              , CssDeclWrapper { property = CssDeclarationBorderTopStyle    CssValueBorderStyleInset, important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightStyle  CssValueBorderStyleInset, important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleInset, important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderLeftStyle   CssValueBorderStyleInset, important = False }

                                              , CssDeclWrapper { property = CssDeclarationBorderTopColor    $ CssValueBorderColor 0x00fff1 , important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightColor  $ CssValueBorderColor 0x00fff1 , important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0x00fff1 , important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderLeftColor   $ CssValueBorderColor 0x00fff1 , important = False }
                                              ])

  , ( "border-top: 5mm solid red",            [ CssDeclWrapper { property = CssDeclarationBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 5)), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderTopStyle CssValueBorderStyleSolid,                           important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderTopColor $ CssValueBorderColor 0xff0000,                     important = False }
                                              ])

  , ( "border-right: 2mm dotted orange",      [ CssDeclWrapper { property = CssDeclarationBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2)), important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightStyle CssValueBorderStyleDotted,                          important = False }
                                              , CssDeclWrapper { property = CssDeclarationBorderRightColor $ CssValueBorderColor 0xffa500,                     important = False }
                                              ])

  , ( "border-bottom: 17px inherit rgb(255, 0, 255)",    [ CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 17)), important = False }
                                                         , CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleInherit,                          important = False }
                                                         , CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0xff00ff,                      important = False }
                                                         ])

  , ( "border-left: 20em none inherit",      [ CssDeclWrapper { property = CssDeclarationBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEm 20)), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftStyle CssValueBorderStyleNone,                             important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftColor $ CssValueBorderColorInherit,                        important = False }
                                             ])

  , ( "border-width: 99px",                  [ CssDeclWrapper { property = CssDeclarationBorderTopWidth    (CssValueBorderWidthDistance (CssDistanceAbsPx 99)), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderRightWidth  (CssValueBorderWidthDistance (CssDistanceAbsPx 99)), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 99)), important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftWidth   (CssValueBorderWidthDistance (CssDistanceAbsPx 99)), important = False }
                                             ])

  , ( "border-style: double",               [ CssDeclWrapper { property = CssDeclarationBorderTopStyle    CssValueBorderStyleDouble,  important = False }
                                            , CssDeclWrapper { property = CssDeclarationBorderRightStyle  CssValueBorderStyleDouble,  important = False }
                                            , CssDeclWrapper { property = CssDeclarationBorderBottomStyle CssValueBorderStyleDouble,  important = False }
                                            , CssDeclWrapper { property = CssDeclarationBorderLeftStyle   CssValueBorderStyleDouble,  important = False }
                                            ])

  , ( "border-color: rgb(0, 0, 255)",        [ CssDeclWrapper { property = CssDeclarationBorderTopColor    $ CssValueBorderColor 0x0000ff,  important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderRightColor  $ CssValueBorderColor 0x0000ff,  important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderBottomColor $ CssValueBorderColor 0x0000ff,  important = False }
                                             , CssDeclWrapper { property = CssDeclarationBorderLeftColor   $ CssValueBorderColor 0x0000ff,  important = False }
                                             ])

  , ( "padding: 1.0px 2.5mm 33.1em 21.9ex",  [ CssDeclWrapper { property = CssDeclarationPaddingTop    (CssValuePadding (CssDistanceAbsPx  1.0)),       important = False }
                                             , CssDeclWrapper { property = CssDeclarationPaddingRight  (CssValuePadding (CssDistanceAbsMm  2.5)),       important = False }
                                             , CssDeclWrapper { property = CssDeclarationPaddingBottom (CssValuePadding (CssDistanceRelEm 33.1)),       important = False }
                                             , CssDeclWrapper { property = CssDeclarationPaddingLeft   (CssValuePadding (CssDistanceRelEx 21.9)),       important = False }
                                             ])



    -- All four values provided: top, right, bottom, left.
  , ("margin: 10.1px 20.2mm 30.3em 40.4ex",  [ CssDeclWrapper { property = CssDeclarationMarginTop    (CssValueMarginDistance (CssDistanceAbsPx 10.1)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginRight  (CssValueMarginDistance (CssDistanceAbsMm 20.2)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceRelEm 30.3)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginLeft   (CssValueMarginDistance (CssDistanceRelEx 40.4)),  important = False }
                                             ])
    -- Three values are provided: top, right-left, bottom.
  , ("margin: 11px 22mm 33.3em",             [ CssDeclWrapper { property = CssDeclarationMarginTop    (CssValueMarginDistance (CssDistanceAbsPx 11.0)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginRight  (CssValueMarginDistance (CssDistanceAbsMm 22.0)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceRelEm 33.3)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginLeft   (CssValueMarginDistance (CssDistanceAbsMm 22.0)),  important = False }
                                             ])
    -- Two values are provided: top-bottom, right-left.
  , ("margin: 100px 200mm",                  [ CssDeclWrapper { property = CssDeclarationMarginTop    (CssValueMarginDistance (CssDistanceAbsPx 100.0)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginRight  (CssValueMarginDistance (CssDistanceAbsMm 200.0)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceAbsPx 100.0)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginLeft   (CssValueMarginDistance (CssDistanceAbsMm 200.0)),  important = False }
                                             ])
    -- One value is provided: top-right-bottom-left.
  , ("margin: 38.01em",                      [ CssDeclWrapper { property = CssDeclarationMarginTop    (CssValueMarginDistance (CssDistanceRelEm 38.01)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginRight  (CssValueMarginDistance (CssDistanceRelEm 38.01)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginBottom (CssValueMarginDistance (CssDistanceRelEm 38.01)),  important = False },
                                               CssDeclWrapper { property = CssDeclarationMarginLeft   (CssValueMarginDistance (CssDistanceRelEm 38.01)),  important = False }
                                             ])




  -- Notice that list-style-image is not supported by the parser and is not tested here.
  , ( "list-style: none",                    [ CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeNone,             important = False }
                                             ])
  , ( "list-style: disc inside",             [ CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeDisc,             important = False }
                                             , CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionInside,   important = False }
                                             ])
  , ( "list-style: outside",                 [ CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionOutside,  important = False }
                                             ])
  , ( "list-style: upper-roman outside",     [ CssDeclWrapper { property = CssDeclarationListStyleType CssValueListStyleTypeUpperRoman,       important = False }
                                             , CssDeclWrapper { property = CssDeclarationListStylePosition CssValueListStylePositionOutside,  important = False }
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
    ((parser', token'), declarations) = parseSingleDeclaration (nextToken2 defaultParser { remainder = rem })




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

