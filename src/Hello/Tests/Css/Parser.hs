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




  , ( "word-spacing: normal !important",    [CssDeclWrapper { property = CssDeclarationWordSpacing CssValueWordSpacingNormal,                                                 important = True  } ])
  , ( "word-spacing: 1.0px",                [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacing (CssValueTypeSignedLength (CssDistanceAbsPx 1.0))),   important = False  } ])
  , ( "word-spacing: 2.5mm !important",     [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacing (CssValueTypeSignedLength (CssDistanceAbsMm 2.5))),   important = True  } ])
  , ( "word-spacing: 3.6em",                [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacing (CssValueTypeSignedLength (CssDistanceRelEm 3.6))),   important = False  } ])
  , ( "word-spacing: 4.7ex !important",     [CssDeclWrapper { property = CssDeclarationWordSpacing (CssValueWordSpacing (CssValueTypeSignedLength (CssDistanceRelEx 4.7))),   important = True  } ])
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

  , ( "padding: 1.0px 2.5mm 33.1em 21.9ex",  [ CssDeclWrapper { property = CssDeclarationPaddingTop    (CssValuePadding (CssDistanceAbsPx  1.0)),       important = False }
                                             , CssDeclWrapper { property = CssDeclarationPaddingRight  (CssValuePadding (CssDistanceAbsMm  2.5)),       important = False }
                                             , CssDeclWrapper { property = CssDeclarationPaddingBottom (CssValuePadding (CssDistanceRelEm 33.1)),       important = False }
                                             , CssDeclWrapper { property = CssDeclarationPaddingLeft   (CssValuePadding (CssDistanceRelEx 21.9)),       important = False }
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

