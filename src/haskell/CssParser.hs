{-
Copyright (C) 2021 Kamil Ignacak acerion@wp.pl

This file is part of "hello" web browser.

"hello" is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

"hello" is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with "hello".  If not, see <https://www.gnu.org/licenses/>.

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-
TODO: be careful with decoding string from external representation into
Data.Text. My original attempt to convert C string into Data.Text in
ffi/CssParser.hsc used T.E.decodeUtf8. I had to change it to T.E.decodeLatin1
because of exceptions from Data.Text module on some characters in some css
files.

Take a look at value of 'content' in this part of css:
a.navmenu::after { content: " â–¶"; }font.logo, font.logobl, img.logo {display: none;}img.sslogo
The line comes from https://lwn.net/CSS/pure-lwn, and the value would lead to
"libEval.so: Cannot decode byte '\xb6': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" error.
-}




{-# LANGUAGE OverloadedStrings #-}




module CssParser(nextToken
                , ignoreBlock
                , ignoreStatement
                , takeSymbol
                , takeInt
                , parseUrl
                , CssParser (..)
                , CssToken (..)
                , tryTakingRgbFunction
                , parseRgbFunction
                , parseRgbFunctionInt

                , declValueAsColor
                , declValueAsString
                , declValueAsEnum
                , declValueAsEnum'
                , declValueAsMultiEnum
                , declValueAsFontWeightInteger
                , declValueAsLength
                , declValueAsURI

                , tokenMatchesProperty

                , cssShorthandInfoIdxByName
                , cssPropertyInfoIdxByName
                , cssPropertyNameString

                , cssLengthType
                , cssLengthValue
                , cssCreateLength

                , invalidIntResult

                , parseDeclarationMultiple
                , parseDeclarationDirections
                , parseDeclarationBorder
                , parseDeclarationShorthand

                , cssShorthandTypeMultiple
                , cssShorthandTypeDirections
                , cssShorthandTypeBorder
                , cssShorthandTypeFont

                , takeLengthTokens

                , defaultSimpleSelector
                , defaultSelector
                , CssSimpleSelector (..)
                , CssSelector (..)
                , takeSelectorTokens
                , parseSelector
                , removeSpaceTokens

                , CssDeclValue (..)
                , parseDeclNormal
                , parseDeclarationWrapper
                , takePropertyTokens2
                , takePropertyTokens
                , parseDeclValue

                , defaultParser) where




--import Prelude
import Data.Maybe
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.IO as T.IO
import qualified HelloUtils as HU
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Bits
import Colors
import HelloUtils
import HtmlTag
import Debug.Trace




data CssToken =
    CssTokI   Int
  | CssTokF   Float
  | CssTokCol T.Text
  | CssTokSym T.Text
  | CssTokStr T.Text
  | CssTokCh  Char
  | CssTokWS          -- Whitespace
  | CssTokEnd         -- End of input. No new tokens will appear in input.
  | CssTokNone        -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show)




data CssParser = CssParser {
    remainder      :: T.Text
  , spaceSeparated :: Bool
  , inBlock        :: Bool
  , bufOffset      :: Int
  } deriving (Show)




defaultParser = CssParser {
    remainder = ""
  , inBlock   = False
  , spaceSeparated = False
  , bufOffset = 0
  }




cssLengthTypeNone       = 0
cssLengthTypePX         = 1
cssLengthTypeMM         = 2 -- "cm", "in", "pt" and "pc" are converted into millimeters.
cssLengthTypeEM         = 3
cssLengthTypeEX         = 4
cssLengthTypePercentage = 5
cssLengthTypeRelative   = 6 -- This does not exist in CSS but is used in HTML
cssLengthTypeAuto       = 7 -- This can be used as a simple value.



invalidIntResult = 99999999


css_background_attachment_enum_vals = ["scroll", "fixed"]
css_background_repeat_enum_vals     = ["repeat", "repeat-x", "repeat-y", "no-repeat"]
css_border_collapse_enum_vals       = ["separate", "collapse"]
css_border_color_enum_vals          = ["transparent"]
css_border_style_enum_vals          = ["none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"]
css_border_width_enum_vals          = ["thin", "medium", "thick"]
css_cursor_enum_vals                = ["crosshair", "default", "pointer", "move", "e-resize", "ne-resize", "nw-resize", "n-resize", "se-resize", "sw-resize", "s-resize", "w-resize", "text", "wait", "help"]
css_display_enum_vals               = ["block", "inline", "inline-block", "list-item", "none", "table", "table-row-group", "table-header-group", "table-footer-group", "table-row", "table-cell"]
css_font_size_enum_vals             = ["large", "larger", "medium", "small", "smaller", "xx-large", "xx-small", "x-large", "x-small"]
css_font_style_enum_vals            = ["normal", "italic", "oblique"]
css_font_variant_enum_vals          = ["normal", "small-caps"]
css_font_weight_enum_vals           = ["bold", "bolder", "light", "lighter", "normal"]
css_letter_spacing_enum_vals        = ["normal"]
css_list_style_position_enum_vals   = ["inside", "outside"]
css_line_height_enum_vals           = ["normal"]
css_list_style_type_enum_vals       = ["disc", "circle", "square", "decimal", "decimal-leading-zero", "lower-roman", "upper-roman", "lower-greek", "lower-alpha", "lower-latin", "upper-alpha", "upper-latin", "hebrew", "armenian", "georgian", "cjk-ideographic", "hiragana", "katakana", "hiragana-iroha", "katakana-iroha", "none"]
css_text_align_enum_vals            = ["left", "right", "center", "justify", "string"]
css_text_decoration_enum_vals       = ["underline", "overline", "line-through", "blink"]
css_text_transform_enum_vals        = ["none", "capitalize", "uppercase", "lowercase"]
css_vertical_align_vals             = ["top", "bottom", "middle", "baseline", "sub", "super", "text-top", "text-bottom"]
css_white_space_vals                = ["normal", "pre", "nowrap", "pre-wrap", "pre-line"]
css_word_spacing_enum_vals          = ["normal"]




type CssDeclValueType = Int

cssDeclValueTypeInt                 =  0 -- This type is only used internally, for x-* properties.
cssDeclValueTypeEnum                =  1 -- Value is i, if represented by enum_symbols[i].
cssDeclValueTypeMultiEnum           =  2 -- For all enum_symbols[i], 1 << i are combined.
cssDeclValueTypeLengthPercent       =  3 -- <length> or <percentage>. Represented by CssLength.
cssDeclValueTypeLength              =  4 -- <length>, represented as CssLength. Note: In some
                                                    -- cases, CSS_TYPE_LENGTH is used instead of
                                                    -- CSS_TYPE_LENGTH_PERCENTAGE, only because Dw cannot
                                                    -- handle percentages in this particular case (e.g.
                                                    -- 'margin-*-width').
cssDeclValueTypeSignedLength        =  5 -- As CSS_TYPE_LENGTH but may be negative.
cssDeclValueTypeLengthPercentNumber =  6 -- <length> or <percentage>, or <number>
cssDeclValueTypeAuto                =  7 -- Represented as CssLength of type cssLengthTypeAuto
cssDeclValueTypeColor               =  8 -- Represented as integer.
cssDeclValueTypeFontWeight          =  9 -- This very special and only used by 'font-weight'
cssDeclValueTypeString              = 10 -- <string>
cssDeclValueTypeSymbol              = 11 -- Symbols, which are directly copied (as opposed to
                                                     -- CSS_PROPERTY_DATA_TYPE_ENUM and
                                                     -- CSS_PROPERTY_DATA_TYPE_MULTI_ENUM). Used for
                                                     -- 'font-family'.
cssDeclValueTypeURI                 = 12 -- <uri>
cssDeclValueTypeBgPosition          = 13 --
cssDeclValueTypeUnused              = 14 -- Not yet used. Will itself get unused some day.




-- CssDeclarationProperty
cssDeclPropertyBackgroundAttachment = 0
cssDeclPropertyBackgroundColor = 1
cssDeclPropertyBackgroundImage = 2
cssDeclPropertyBackgroundPosition = 3
cssDeclPropertyBackgroundRepeat = 4
cssDeclPropertyBorderBottomColor = 5
cssDeclPropertyBorderBottomStyle = 6
cssDeclPropertyBorderBottomWidth = 7
cssDeclPropertyBorderCollapse = 8
cssDeclPropertyBorderLeftColor = 9
cssDeclPropertyBorderLeftStyle = 10
cssDeclPropertyBorderLeftWidth = 11
cssDeclPropertyBorderRightColor = 12
cssDeclPropertyBorderRightStyle = 13
cssDeclPropertyBorderRightWidth = 14
cssDeclPropertyBorderSpacing = 15
cssDeclPropertyBorderTopColor = 16
cssDeclPropertyBorderTopStyle = 17
cssDeclPropertyBorderTopWidth = 18
cssDeclPropertyBottom = 19
cssDeclPropertyCaptionSide = 20
cssDeclPropertyClear = 21
cssDeclPropertyClip = 22
cssDeclPropertyColor = 23
cssDeclPropertyContent = 24
cssDeclPropertyCounterIncrement = 25
cssDeclPropertyCounterReset = 26
cssDeclPropertyCursor = 27
cssDeclPropertyDirection = 28
cssDeclPropertyDisplay = 29
cssDeclPropertyEmptyCells = 30
cssDeclPropertyFlaot = 31
cssDeclPropertyFontFamily = 32
cssDeclPropertyFontSize = 33
cssDeclPropertyFontSizeAdjust = 34
cssDeclPropertyFontStretch = 35
cssDeclPropertyFontStyle = 36
cssDeclPropertyFontVariant = 37
cssDeclPropertyFontWeight = 38
cssDeclPropertyHeight = 39
cssDeclPropertyLeft = 40
cssDeclPropertyLetterSpacing = 41
cssDeclPropertyLineHeight = 42
cssDeclPropertyListStyleImage = 43
cssDeclPropertyListStylePosition = 44
cssDeclPropertyListStyleType = 45
cssDeclPropertyMarginBottom = 46
cssDeclPropertyMarginLeft = 47
cssDeclPropertyMarginRight = 48
cssDeclPropertyMarginTop = 49
cssDeclPropertyMarkerOffset = 50
cssDeclPropertyMarks = 51
cssDeclPropertyMaxHeight = 52
cssDeclPropertyMaxWidth = 53
cssDeclPropertyMinHeight = 54
cssDeclPropertyMinWidth = 55
cssDeclPropertyOutlineColor = 56
cssDeclPropertyOutlineStyle = 57
cssDeclPropertyOutlineWidth = 58
cssDeclPropertyOverflow = 59
cssDeclPropertyPaddingBottom = 60
cssDeclPropertyPaddingLeft = 61
cssDeclPropertyPaddingRight = 62
cssDeclPropertyPaddingTop = 63
cssDeclPropertyPosition = 64
cssDeclPropertyQuotes = 65
cssDeclPropertyRight = 66
cssDeclPropertyTextAlign = 67
cssDeclPropertyTextDecoration = 68
cssDeclPropertyTextIndent = 69
cssDeclPropertyTextShadow = 70
cssDeclPropertyTextTransform = 71
cssDeclPropertyTop = 72
cssDeclPropertyUnicodeBiDi = 73
cssDeclPropertyVerticalAlign = 74
cssDeclPropertyVisibility = 75
cssDeclPropertyWhitespace = 76
cssDeclPropertyWidth = 77
cssDeclPropertyWordSpacing = 78
cssDeclPropertyZIndex = 79
cssDeclPropertyXLink = 80
cssDeclPropertyXColSpan = 81
cssDeclPropertyXRowSpan = 82

cssPropertyXLink = 83
cssPropertyXLang = 84
cssPropertyXImg = 85
cssPropertyXTooltip = 86

cssDeclProperty_LAST = 87




cssPropertyInfo = V.fromList [
     ("background-attachment",  [ cssDeclValueTypeEnum ],                                             css_background_attachment_enum_vals)
   , ("background-color",       [ cssDeclValueTypeColor ],                                            [])
   , ("background-image",       [ cssDeclValueTypeURI ],                                              [])
   , ("background-position",    [ cssDeclValueTypeBgPosition ],                                       [])
   , ("background-repeat",      [ cssDeclValueTypeEnum ],                                             css_background_repeat_enum_vals)
   , ("border-bottom-color",    [ cssDeclValueTypeEnum, cssDeclValueTypeColor ],                      css_border_color_enum_vals)
   , ("border-bottom-style",    [ cssDeclValueTypeEnum ],                                             css_border_style_enum_vals)
   , ("border-bottom-width",    [ cssDeclValueTypeEnum, cssDeclValueTypeLength ],                     css_border_width_enum_vals)
   , ("border-collapse",        [ cssDeclValueTypeEnum ],                                             css_border_collapse_enum_vals)
   , ("border-left-color",      [ cssDeclValueTypeEnum, cssDeclValueTypeColor ],                      css_border_color_enum_vals)
   , ("border-left-style",      [ cssDeclValueTypeEnum ],                                             css_border_style_enum_vals)
   , ("border-left-width",      [ cssDeclValueTypeEnum, cssDeclValueTypeLength ],                     css_border_width_enum_vals)
   , ("border-right-color",     [ cssDeclValueTypeEnum, cssDeclValueTypeColor ],                      css_border_color_enum_vals)
   , ("border-right-style",     [ cssDeclValueTypeEnum ],                                             css_border_style_enum_vals)
   , ("border-rigth-width",     [ cssDeclValueTypeEnum, cssDeclValueTypeLength ],                     css_border_width_enum_vals)
   , ("border-spacing",         [ cssDeclValueTypeLength ],                                           [])
   , ("border-top-color",       [ cssDeclValueTypeEnum, cssDeclValueTypeColor ],                      css_border_color_enum_vals)
   , ("border-top-style",       [ cssDeclValueTypeEnum ],                                             css_border_style_enum_vals)
   , ("border-top-width",       [ cssDeclValueTypeEnum, cssDeclValueTypeLength ],                     css_border_width_enum_vals)
   , ("bottom",                 [],                                                                   [])
   , ("caption-side",           [],                                                                   [])
   , ("clear",                  [],                                                                   [])
   , ("clip",                   [],                                                                   [])
   , ("color",                  [ cssDeclValueTypeColor ],                                            [])
   , ("content",                [ cssDeclValueTypeString ],                                           [])
   , ("counter-increment",      [],                                                                   [])
   , ("counter-reset",          [],                                                                   [])
   , ("cursor",                 [ cssDeclValueTypeEnum ],                                             css_cursor_enum_vals)
   , ("direction",              [],                                                                   [])
   , ("display",                [ cssDeclValueTypeEnum ],                                             css_display_enum_vals)
   , ("empty-cells",            [],                                                                   [])
   , ("float",                  [],                                                                   [])
   , ("font-family",            [ cssDeclValueTypeSymbol ],                                           [])
   , ("font-size",              [ cssDeclValueTypeEnum, cssDeclValueTypeLengthPercent ],              css_font_size_enum_vals)
   , ("font-size-adjust",       [],                                                                   [])
   , ("font-stretch",           [],                                                                   [])
   , ("font-style",             [ cssDeclValueTypeEnum ],                                             css_font_style_enum_vals)
   , ("font-variant",           [ cssDeclValueTypeEnum ],                                             css_font_variant_enum_vals)
   , ("font-weight",            [ cssDeclValueTypeEnum, cssDeclValueTypeFontWeight ],                 css_font_weight_enum_vals)
   , ("height",                 [ cssDeclValueTypeLengthPercent, cssDeclValueTypeAuto ],              [])
   , ("left",                   [],                                                                   [])
   , ("letter-spacing",         [ cssDeclValueTypeEnum, cssDeclValueTypeSignedLength ],               css_letter_spacing_enum_vals)
   , ("line-height",            [ cssDeclValueTypeEnum, cssDeclValueTypeLengthPercentNumber ],        css_line_height_enum_vals)
   , ("list-style-image",       [],                                                                   [])
   , ("list-style-position",    [ cssDeclValueTypeEnum ],                                             css_list_style_position_enum_vals)
   , ("list-style-type",        [ cssDeclValueTypeEnum ],                                             css_list_style_type_enum_vals)
   , ("margin-bottom",          [ cssDeclValueTypeSignedLength, cssDeclValueTypeAuto ],               [])
   , ("margin-left",            [ cssDeclValueTypeSignedLength, cssDeclValueTypeAuto ],               [])
   , ("margin-right",           [ cssDeclValueTypeSignedLength, cssDeclValueTypeAuto ],               [])
   , ("margin-top",             [ cssDeclValueTypeSignedLength, cssDeclValueTypeAuto ],               [])
   , ("marker-offset",          [],                                                                   [])
   , ("marks",                  [],                                                                   [])
   , ("max-height",             [],                                                                   [])
   , ("max-width",              [],                                                                   [])
   , ("min-height",             [],                                                                   [])
   , ("min-width",              [],                                                                   [])
   , ("outline-color",          [],                                                                   [])
   , ("outline-style",          [],                                                                   [])
   , ("outline-width",          [],                                                                   [])
   , ("overflow",               [],                                                                   [])
   , ("padding-bottom",         [ cssDeclValueTypeLength ],                                           [])
   , ("padding-left",           [ cssDeclValueTypeLength ],                                           [])
   , ("padding-right",          [ cssDeclValueTypeLength ],                                           [])
   , ("padding-top",            [ cssDeclValueTypeLength ],                                           [])
   , ("position",               [],                                                                   [])
   , ("quotes",                 [],                                                                   [])
   , ("right",                  [],                                                                   [])
   , ("text-align",             [ cssDeclValueTypeEnum ],                                             css_text_align_enum_vals)
   , ("text-decoration",        [ cssDeclValueTypeMultiEnum ],                                        css_text_decoration_enum_vals)
   , ("text-indent",            [ cssDeclValueTypeLengthPercent ],                                    [])
   , ("text-shadow",            [],                                                                   [])
   , ("text-transform",         [ cssDeclValueTypeEnum ],                                             css_text_transform_enum_vals)
   , ("top",                    [],                                                                   [])
   , ("unicode-bidi",           [],                                                                   [])
   , ("vertical-align",         [ cssDeclValueTypeEnum ],                                             css_vertical_align_vals)
   , ("visibility",             [],                                                                   [])
   , ("white-space",            [ cssDeclValueTypeEnum ],                                             css_white_space_vals)
   , ("width",                  [ cssDeclValueTypeLengthPercent, cssDeclValueTypeAuto ],              [])
   , ("word-spacing",           [ cssDeclValueTypeEnum, cssDeclValueTypeSignedLength ],               css_word_spacing_enum_vals)
   , ("z-index",                [],                                                                   [])

   -- These are extensions, for internal used, and never parsed.
   -- TODO: verify whether we need them.
   -- TODO: verify if we still need "last" property.
   , ("x-link",                 [ cssDeclValueTypeInt ],                                              [])
   , ("x-colspan",              [ cssDeclValueTypeInt ],                                              [])
   , ("x-rowspan",              [ cssDeclValueTypeInt ],                                              [])
   , ("last",                   [], [])
   ] :: V.Vector (T.Text, [CssDeclValueType], [T.Text])




cssShorthandTypeMultiple   = 0 -- [ p1 || p2 || ...], the property pi is determined  by the type; array of properties must be terminated by CSS_PROPERTY_End.
cssShorthandTypeDirections = 1 --  <t>{1,4}; array of properties must have length 4.
cssShorthandTypeBorder     = 2 -- special, used for 'border'; array of properties must have length 12.
cssShorthandTypeFont       = 3 -- special, used for 'font'




cssShorthandInfo = V.fromList [
    ("background",     cssShorthandTypeMultiple,      [ cssDeclPropertyBackgroundColor, cssDeclPropertyBackgroundImage, cssDeclPropertyBackgroundRepeat,
                                                        cssDeclPropertyBackgroundAttachment, cssDeclPropertyBackgroundPosition ])

  , ("border",         cssShorthandTypeBorder,        [ cssDeclPropertyBorderTopWidth, cssDeclPropertyBorderRightWidth, cssDeclPropertyBorderBottomWidth, cssDeclPropertyBorderLeftWidth,
                                                        cssDeclPropertyBorderTopStyle, cssDeclPropertyBorderRightStyle, cssDeclPropertyBorderBottomStyle, cssDeclPropertyBorderLeftStyle,
                                                        cssDeclPropertyBorderTopColor, cssDeclPropertyBorderRightColor, cssDeclPropertyBorderBottomColor, cssDeclPropertyBorderLeftColor])

  , ("border-bottom",  cssShorthandTypeMultiple,      [ cssDeclPropertyBorderBottomWidth, cssDeclPropertyBorderBottomStyle,  cssDeclPropertyBorderBottomColor ])
  , ("border-color",   cssShorthandTypeDirections,    [ cssDeclPropertyBorderTopColor,    cssDeclPropertyBorderRightColor,   cssDeclPropertyBorderBottomColor, cssDeclPropertyBorderLeftColor ])
  , ("border-left",    cssShorthandTypeMultiple,      [ cssDeclPropertyBorderLeftWidth,   cssDeclPropertyBorderLeftStyle,    cssDeclPropertyBorderLeftColor ])
  , ("border-right",   cssShorthandTypeMultiple,      [ cssDeclPropertyBorderRightWidth,  cssDeclPropertyBorderRightStyle,   cssDeclPropertyBorderRightColor ])
  , ("border-style",   cssShorthandTypeDirections,    [ cssDeclPropertyBorderTopStyle,    cssDeclPropertyBorderRightStyle,   cssDeclPropertyBorderBottomStyle, cssDeclPropertyBorderLeftStyle ])
  , ("border-top",     cssShorthandTypeMultiple,      [ cssDeclPropertyBorderTopWidth,    cssDeclPropertyBorderTopStyle,     cssDeclPropertyBorderTopColor ])

  , ("border-width",   cssShorthandTypeDirections,    [ cssDeclPropertyBorderTopWidth,    cssDeclPropertyBorderRightWidth,   cssDeclPropertyBorderBottomWidth, cssDeclPropertyBorderLeftWidth ])

  , ("font",           cssShorthandTypeFont,          [ cssDeclPropertyFontSize,  cssDeclPropertyFontStyle, cssDeclPropertyFontVariant, cssDeclPropertyFontWeight, cssDeclPropertyFontFamily ])

  , ("list-style",     cssShorthandTypeMultiple,      [ cssDeclPropertyListStyleType, cssDeclPropertyListStylePosition, cssDeclPropertyListStyleImage ])
  , ("margin",         cssShorthandTypeDirections,    [ cssDeclPropertyMarginTop,         cssDeclPropertyMarginRight,        cssDeclPropertyMarginBottom,      cssDeclPropertyMarginLeft ])
  , ("outline",        cssShorthandTypeMultiple,      [ cssDeclPropertyOutlineColor, cssDeclPropertyOutlineStyle, cssDeclPropertyOutlineWidth])

  , ("padding",        cssShorthandTypeDirections,    [ cssDeclPropertyPaddingTop,        cssDeclPropertyPaddingRight,       cssDeclPropertyPaddingBottom,     cssDeclPropertyPaddingLeft ])
  ] :: V.Vector (T.Text, Int, [Int])




-- TODO: case-insensitive search
cssShorthandInfoIdxByName :: T.Text -> Maybe Int
cssShorthandInfoIdxByName shorthandName = V.findIndex p cssShorthandInfo
  where
    p :: (T.Text, Int, [Int]) -> Bool
    p = (\t -> (tripletFst t) == shorthandName)




nextToken :: CssParser -> (CssParser, CssToken)
nextToken parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = nextToken' parser{spaceSeparated = False}
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)



nextToken2 :: CssParser -> (CssParser, CssToken)
nextToken2 parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = nextToken2' parser{spaceSeparated = False}
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)




-- This function is based on function with the same name from Real World
-- Haskell, chapter 10.
--
-- These lines are most awesome piece of code that I've written so far, in
-- any project.
(>>?) :: (CssParser, CssToken) -> (CssParser -> (CssParser, CssToken)) -> (CssParser, CssToken)
(parser, CssTokNone) >>? f = f parser
pair@(parser, _) >>? _     = pair





-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
nextToken' :: CssParser -> (CssParser, CssToken)
nextToken' parser = takeLeadingWhite parser >>?
                    takeFloat               >>?
                    takeInt                 >>?
                    takeSymbol              >>?
                    takeString              >>?
                    takeColor               >>?
                    takeCharacter




-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
nextToken2' :: CssParser -> (CssParser, CssToken)
nextToken2' parser = takeLeadingWhite2 parser >>?
                     takeFloat                >>?
                     takeInt                  >>?
                     takeSymbol               >>?
                     takeString               >>?
                     takeColor                >>?
                     takeCharacter




-- Symbol must not start with a digit, therefore we have to have some kind of
-- test at the beginning. TODO: can we do the test in a better way?
--
-- TODO: Original C code parsed symbols starting with '-' (such as
-- "-webkit-user-select") in a way that resulted in token without the leading
-- '-' (so the resulting token was "webkit-user-select"). Haskell code keeps
-- the leading '-' character.
takeSymbol :: CssParser -> (CssParser, CssToken)
takeSymbol parser = if predNonNumeric . T.head . remainder $ parser
                    then (parserAppend parser tok, CssTokSym tok)
                    else (parser, CssTokNone)
  where tok = T.takeWhile pred (remainder parser)
        predNonNumeric = (\c -> D.C.isAlpha c || c == '_' || c == '-')
        pred = (\c -> D.C.isAlphaNum c || c == '_' || c == '-')




{-
TODO: handle escaped sequences in string.
Examples of escaped sequences:

before{content:"Poka\00017C  wi\000119cej";position:relative;top:-5px}
token: "Poka7C  wi19cej", [50 6f 6b 61 01 37 43 20 20 77 69 01 31 39 63 65 6a ]

before{content:"Na \00017Bywo"}
token: "Na 7Bywo", [4e 61 20 01 37 42 79 77 6f ]

.icon--arrow-right:before{content:"\f107"}
token  = "", [07 ]
-}
takeString :: CssParser -> (CssParser, CssToken)
takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> (parser, CssTokNone)
  where
    parseString :: CssParser -> T.Text -> T.Text -> (CssParser, CssToken)
    parseString parser string rem = (parser{remainder = rem}, CssTokStr (escapedString string))
    escapedString str = case T.findIndex (== '\\') str of
                          Just i -> ""
                          Nothing -> str




-- TODO: think about performance of using isPrefixOf to get just one
-- character, here and elsewhere.
takeColor :: CssParser -> (CssParser, CssToken)
takeColor parser = if T.isPrefixOf "#" (remainder parser) && (inBlock parser)
                   then takeColor' parser
                   else (parser, CssTokNone) -- Don't take the leading '#' if we are not in a block

  where
    takeColor' :: CssParser -> (CssParser, CssToken)
    takeColor' parser = (parser{ remainder = newRem }
                        , CssTokCol (T.concat ["#", newValue ]))
                        -- TODO: verify if we really need the leading '#' in token.
                        -- TODO: what if there are no digits after '#'?
                        -- TODO: add better handling of '#' followed by non-hex string.

      where
        newValue = T.takeWhile D.C.isHexDigit digitsString
        newRem = T.dropWhile D.C.isHexDigit digitsString
        digitsString = T.drop 1 (remainder parser)




takeCharacter :: CssParser -> (CssParser, CssToken)
takeCharacter parser = if T.null . remainder $ parser
                       then (parser, CssTokNone)
                       else (parserAppend parser (T.singleton . T.head . remainder $ parser),
                             CssTokCh (T.head . remainder $ parser))




-- This function does not return a token. Discarding meaningless data from
-- beginning of text would not create a valid token.
takeLeadingWhite :: CssParser -> (CssParser, CssToken)
takeLeadingWhite parser
  | T.null rem                 = (parser, CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = (parser, CssTokNone)
  where rem = remainder parser




-- This function may complete withouth returning a valid token. Discarding
-- meaningless data from beginning of text would not create a valid token.
takeLeadingWhite2 :: CssParser -> (CssParser, CssToken)
takeLeadingWhite2 parser
  | T.null rem                 = (parser, CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite2 parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite2 parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite2 parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = if (not . inBlock $ parser) && spaceSeparated parser
                                 then (parser, CssTokWS)
                                 else (parser, CssTokNone)
  where rem = remainder parser




parserAppend :: CssParser -> T.Text -> CssParser
parserAppend parser tok = parser { remainder = T.drop (T.length tok) (remainder parser) }




-- TODO: this function doesn't recognize some float formats that are valid in
-- CSS, e.g. ".5".
--
-- T.R.rational is happy to interpret "100" as float, but we want to treat is
-- as int. Therefore we have to search for '.' in taken sub-string :(
takeFloat :: CssParser -> (CssParser, CssToken)
takeFloat parser = case T.R.signed T.R.rational (remainder parser) of
                     Right pair -> case T.find (== '.') val of
                                     Just c    -> (parserAppend parser val, CssTokF (fst pair))
                                     otherwise -> (parser, CssTokNone)
                       where
                         val = T.take diff (remainder parser)
                         newRem = snd pair
                         diff = (T.length . remainder $ parser) - (T.length newRem)
                     Left pair -> (parser, CssTokNone)




-- This function is very similar to takeFloat, but I don't want to write a
-- common function just yet. takeFloat will have to be updated to read all
-- formats of float value, and that change may make it more complicated and
-- less similar to takeInt.
takeInt :: CssParser -> (CssParser, CssToken)
takeInt parser = case T.R.signed T.R.decimal (remainder parser) of
                   Right pair -> (parserAppend parser val, CssTokI (fst pair))
                     where
                       val = T.take diff (remainder parser)
                       newRem = snd pair
                       diff = (T.length . remainder $ parser) - (T.length newRem)
                   Left pair -> (parser, CssTokNone)




{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




parseRgbFunctionInt :: CssParser -> ((CssParser, CssToken), Maybe Int)
parseRgbFunctionInt parser =
  case parseRgbFunction parser of
    (parser', Nothing) -> ((parser', CssTokNone), Nothing) -- TODO: return real token here
    (parser', Just (tr, tg, tb, ta, isPercent)) -> ((parser', CssTokNone), Just color) -- TODO: return real token here
      where
        color = (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
        r = getInt tr isPercent
        g = getInt tg isPercent
        b = getInt tb isPercent

        -- TODO: make sure that r/g/b values are in range 0-255.
        getInt :: Int -> Bool -> Int
        getInt i isPercent = if isPercent then (i * 255) `div` 100 else i




parseRgbFunction :: CssParser -> (CssParser, Maybe (Int, Int, Int, T.Text, Bool))
parseRgbFunction parser =
  let fun = tryTakingRgbFunction $ parser
      parser' = fst fun
      tokens = snd fun :: [CssToken]
  in
    case tokens of
      (CssTokCh par2:CssTokCh perc3:CssTokI b:CssTokCh comma2:CssTokCh perc2:CssTokI g:CssTokCh comma1:CssTokCh perc1:CssTokI r:CssTokCh par1:[]) ->
        if par1 == '(' && par2 == ')' && perc3 == '%' && perc2 == '%' && perc1 == '%' && comma2 == ',' && comma1 == ','
        then (parser', Just (r, g, b, "%", True))
        else (parser', Nothing)
      (CssTokCh par2:CssTokI b:CssTokCh comma2:CssTokI g:CssTokCh comma1:CssTokI r:CssTokCh par1:[]) ->
        if par1 == '(' && par2 == ')' && comma2 == ',' && comma1 == ','
        then (parser', Just (r, g, b, "/100", False))
        else (parser', Nothing)
      otherwise -> (parser', Nothing)




tryTakingRgbFunction :: CssParser -> (CssParser, [CssToken])
tryTakingRgbFunction parser = takeNext parser []
  where
    takeNext :: CssParser -> [CssToken] -> (CssParser, [CssToken])
    takeNext parser list@(CssTokCh c:xs) = if length list == 10 || (c == ')' && length list == 7)
                                           then (nextParser, list)
                                           else takeNext nextParser (tok:list)
      where (nextParser, tok) = nextToken parser

    takeNext parser []        = takeNext nextParser [tok]
      where (nextParser, tok) = nextToken parser

    takeNext parser list                 = if length list == 10
                                           then (nextParser, list)
                                           else takeNext nextParser (tok:list)
      where (nextParser, tok) = nextToken parser
-- TODO: handle invalid token type here




{-
-- TODO: move getting leading minus to takeNumber. Have a clearer distinction
-- between minus being a part of a number and minus being a part of other
-- type of token. Make sure that this call: "nextToken
-- defaultParser{remainder="/* hello */ -"}" returns token type == TokenChar.
-- Or should it be treated as invalid?
takeLeadingMinus :: CssParser -> (CssParser, CssToken)
takeLeadingMinus parser = case T.uncons (remainder parser) of
                            Just (c, rem) | c == '-'  -> (parser{remainder = rem}, CssToken (T.singleton c) Nothing)
                                          | otherwise -> (parser, CssTokNone)
                            Nothing -> (parser, CssTokNone)
-}





declValueAsColor :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsColor (parser, token@(CssTokCol c)) = case colorsStringToColor c of -- TODO: we know here that color should have form #RRGGBB. Call function that accepts only this format.
                                                   Just i  -> (nextToken parser, Just defaultValue{typeTag = cssDeclValueTypeColor, intVal = i})
                                                   Nothing -> (nextToken parser, Nothing)
declValueAsColor (parser, token@(CssTokSym s)) | s == "rgb" = case parseRgbFunctionInt parser of
                                                                ((p, t), Just c)  -> ((p, t), Just defaultValue{typeTag = cssDeclValueTypeColor, intVal = c})
                                                                ((p, t), Nothing) -> ((p, t), Nothing)
                                               | otherwise = case colorsStringToColor s of
                                                               Just i  -> (nextToken parser, Just defaultValue{typeTag = cssDeclValueTypeColor, intVal = i})
                                                               Nothing -> (nextToken parser, Nothing)
declValueAsColor (parser, token)               = ((parser, token), Nothing)





declValueAsString :: (CssParser, CssToken) -> Int -> Int -> ((CssParser, CssToken), Maybe T.Text)
declValueAsString (parser, token) valueType property = case ((retParser, retToken), value) of
                                                         ((p, t), Just v)  -> ((p, t), Just (textVal v))
                                                         ((p, t), Nothing) -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | valueType == cssDeclValueTypeString = declValueAsString' (parser, token)
                                   | valueType == cssDeclValueTypeSymbol = declValueAsSymbol (parser, token) ""
                                   | valueType == cssDeclValueTypeURI    = declValueAsURI (parser, token)
                                   | otherwise                           = ((parser, token), Nothing)

{-
  | valueType == cssDeclValueTypeInt                  = (parser, Nothing)

  | valueType == cssDeclValueTypeBgPosition      = (parser, Nothing)
  | valueType == cssDeclValueTypeUnused                   = (parser, Nothing)
-}





declValueAsEnum :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsEnum (parser, token@(CssTokSym symbol)) property =
  case declValueAsEnum' symbol enums 0 of
    -1  -> ((parser, token), Nothing)
    idx -> (nextToken parser, Just defaultValue{typeTag = cssDeclValueTypeEnum, intVal = idx})
  where
    propInfo = cssPropertyInfo V.! property
    enums = tripletThrd propInfo
declValueAsEnum (parser, token) property                    = ((parser, token), Nothing)
                                                                         -- TODO: is this the right place to reject everything else other than symbol?
                                                                         -- Shouldn't we do it somewhere else?



-- TODO: can't we use Data.List.elemIndex here?
declValueAsEnum' :: T.Text -> [T.Text] -> Int -> Int
declValueAsEnum' symbol []     idx = -1
declValueAsEnum' symbol (x:xs) idx = if x == symbol
                                     then idx
                                     else declValueAsEnum' symbol xs (idx + 1)




declValueAsMultiEnum :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsMultiEnum (parser, token@(CssTokSym symbol)) property = declValueAsMultiEnum' (parser, token) enums 0
  where
    propInfo = cssPropertyInfo V.! property
    enums = tripletThrd propInfo
declValueAsMultiEnum (parser, token) property                    = ((parser, token), Nothing)
                                                            -- TODO: is this the right place to reject everything else other than symbol?
                                                            -- Shouldn't we do it somewhere else?




declValueAsMultiEnum' :: (CssParser, CssToken) -> [T.Text] -> Int -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsMultiEnum' (parser, (CssTokSym symbol)) (enums) bits =
  case L.elemIndex symbol enums of -- TODO: this search should be case-insensitive
    Just pos -> declValueAsMultiEnum' (newParser, newToken) enums (bits .|. (1  `shiftL` pos))
    Nothing  -> declValueAsMultiEnum' (newParser, newToken) enums bits
  where
    (newParser, newToken) = nextToken parser
declValueAsMultiEnum' (parser, token) _ bits                    = ((parser, token), Just defaultValue{typeTag = cssDeclValueTypeMultiEnum, intVal = bits})
-- TODO: we should probably handle in a different way a situation where one
-- of tokens is not a symbol.
--
-- TOOO: symbol "none" should be handled in special way (probably).




-- TODO: check value of symbol (case insensitive): it should be "auto".
declValueAsAuto :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsAuto (parser, token@(CssTokSym symbol)) = ((nextToken parser), Just defaultValue{typeTag = cssDeclValueTypeAuto, intVal = cssLengthTypeAuto})
declValueAsAuto (parser, token)                    = ((parser, token), Nothing)




lengthValueToValAndType :: Float -> T.Text -> (Float, Int)
lengthValueToValAndType fval unitStr | unitStr == "px" = (fval,               cssLengthTypePX)
                                     | unitStr == "mm" = (fval,               cssLengthTypeMM)
                                     | unitStr == "cm" = (fval * 10,          cssLengthTypeMM)
                                     | unitStr == "in" = (fval * 25.4,        cssLengthTypeMM)
                                     | unitStr == "pt" = (fval * (25.4/72.0), cssLengthTypeMM)
                                     | unitStr == "pc" = (fval * (25.4/6.0),  cssLengthTypeMM)
                                     | unitStr == "em" = (fval,               cssLengthTypeEM)
                                     | unitStr == "ex" = (fval,               cssLengthTypeEX)
                                     | otherwise       = (fval,               cssLengthTypeNone)




-- TODO: this function should handle multiple values per property, like here:
-- "th{border-width:0 0 1px;".
takeLengthTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeLengthTokens (parser, token) = if isSpaceSeparated parser
                                   then case token of
                                          CssTokF _ -> (nextToken parser, [token])
                                          CssTokI _ -> (nextToken parser, [token])
                                          _         -> (nextToken parser, [])
                                   else case token of
                                          CssTokF _ -> numberWithSomething (parser, token)
                                          CssTokI _ -> numberWithSomething (parser, token)
                                          _         -> (nextToken parser, [])

  where
    numberWithSomething (parser, numberToken) = case nextToken parser of
                                                  pair@(p3, CssTokSym sym)  -> if unitStringIsValid sym
                                                                               then (nextToken p3, [numberToken, snd pair])
                                                                               else (nextToken p3, []) -- TODO: how to handle unrecognized symbol?
                                                  pair@(p3, CssTokCh '%') -> (nextToken p3, [numberToken, snd pair])
                                                  pair@(p3, CssTokCh ';') -> (pair, [numberToken])
                                                  pair@(p3, CssTokCh '}') -> (pair, [numberToken])
                                                  pair@(p3, CssTokEnd)    -> (pair, [numberToken])
                                                  pair                    -> ((parser, token), [])

    unitStringIsValid str = str == "px" || str == "mm" || str == "cm" || str == "in" || str == "pt" || str == "pc" || str == "em" || str == "ex"




isSpaceSeparated parser = spaceSeparated newParser
  where (newParser, newToken) = nextToken parser




declValueAsLength :: (CssParser, CssToken) -> CssDeclValueType -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsLength (parser, token) valueType =
  case tokens of
    [CssTokF f, CssTokSym sym] -> ((newParser, newToken), unitValue sym valueType f)
    [CssTokI i, CssTokSym sym] -> ((newParser, newToken), unitValue sym valueType (fromIntegral i))
    [CssTokF f, CssTokCh '%']  -> ((newParser, newToken), percentValue valueType f)
    [CssTokI i, CssTokCh '%']  -> ((newParser, newToken), percentValue valueType (fromIntegral i))
    [CssTokF f]                -> ((newParser, newToken), unitlessValue valueType f)
    [CssTokI i]                -> ((newParser, newToken), unitlessValue valueType (fromIntegral i))
    _                          -> ((parser, token), Nothing)
  where
    ((newParser, newToken), tokens) = takeLengthTokens (parser, token)

    percentValue :: CssDeclValueType -> Float -> Maybe CssDeclValue
    percentValue valueType fval = Just defaultValue{typeTag = valueType, intVal = (cssCreateLength val t)}
      where
        (val, t) = ((fval / 100.0), cssLengthTypePercentage)

    unitValue :: T.Text -> CssDeclValueType -> Float -> Maybe CssDeclValue
    unitValue unitString valueType fval = Just defaultValue{typeTag = valueType, intVal = (cssCreateLength val t)}
      where
        (val, t) = lengthValueToValAndType fval (T.toLower unitString)

    unitlessValue :: CssDeclValueType -> Float -> Maybe CssDeclValue
    -- Allow numbers without unit only for 0 or cssDeclValueTypeLengthPercentNumber. TODO: why?
    unitlessValue valueType fval = if (valueType == cssDeclValueTypeLengthPercentNumber || fval == 0.0)
                                   then Just defaultValue{typeTag = valueType, intVal = (cssCreateLength val t)}
                                   else Nothing
      where
        (val, t) = (fval, cssLengthTypeNone)




declValueAsString' :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsString' (parser, (CssTokStr s)) = ((newParser, newToken), Just defaultValue{typeTag = cssDeclValueTypeString, textVal = s})
  where (newParser, newToken) = nextToken parser
declValueAsString' (parser, token)         = ((parser, token), Nothing)




declValueAsURI :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsURI (parser, token) = case parseUrl (parser, token) of
                                   ((newParser, newToken), Just url) -> ((newParser, newToken), Just defaultValue{typeTag = cssDeclValueTypeURI, textVal = url})
                                   ((newParser, newToken), Nothing)  -> ((newParser, newToken), Nothing)




parseUrl :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
parseUrl (parser, token@(CssTokSym "url")) = (outParser, outUrl)
  where
    outUrl = case partialUrl of
      Nothing  -> Nothing
      Just url -> Just url -- TODO: here we have to add first part of URL, defined in CssParser (the one starting with e.g. http://server.com).
    (outParser, partialUrl) = case nextToken parser of
                                (newParser, newToken@(CssTokCh '(')) -> appendToUrl (newParser, newToken) ""
                                (newParser, newToken)                -> ((parser, token), Nothing)
    appendToUrl (parser, token) acc = case nextToken parser of
                                        pair@(newParser, CssTokCh ')')  -> (pair, Just acc)
                                        pair@(newParser, CssTokCh ch)   -> appendToUrl pair (T.snoc acc ch)
                                        pair@(newParser, CssTokStr str) -> appendToUrl pair (T.concat [acc, str])
                                        pair@(newParser, CssTokSym str) -> appendToUrl pair (T.concat [acc, str])
                                        pair@(newParser, _)             -> (pair, Nothing) -- TODO: This is a BAD URL situation
parseUrl (parser, token)   = ((parser, token), Nothing)






-- Read comma separated list of font family names.
-- TODO: test the code for list of symbols separated by space or comma.
declValueAsSymbol :: (CssParser, CssToken) -> T.Text -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsSymbol (parser, (CssTokSym sym)) acc = declValueAsSymbol (nextToken parser) (T.append acc (separated parser sym))
declValueAsSymbol (parser, (CssTokStr str)) acc = declValueAsSymbol (nextToken parser) (T.append acc (separated parser str))
declValueAsSymbol (parser, (CssTokCh  ',')) acc = declValueAsSymbol (nextToken parser) (T.append acc (separated parser ","))
declValueAsSymbol (parser, token) acc           = finalSymbol (parser, token) acc




finalSymbol (parser, token)  acc = if T.null acc
                                   then ((parser, token), Nothing)
                                   else ((parser, token), Just defaultValue{typeTag = cssDeclValueTypeSymbol, textVal = acc})

-- TODO: check if CSS code really needs this space. In some situations
-- symbols in text returned by declValueAsSymbol may be separated by
-- comma AND space, which may be redundant.
separated parser str = if spaceSeparated parser
                       then T.cons ' ' str
                       else str


{-
   case CssDeclValueTypeSYMBOL:

      dstr = dStr_new("");
      while (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL || tokenizer.type == CSS_TOKEN_TYPE_STRING ||
             (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ',')) {
         if (this->hll_css_parser.spaceSeparatedC)
            dStr_append_c(dstr, ' ');
         dStr_append(dstr, tokenizer.value);
         ret = true;
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }

      if (ret) {
         value->strVal = dStrstrip(dstr->str);
         dStr_free(dstr, 0);
      } else {
         dStr_free(dstr, 1);
      }
      break;

-}


declValueAsFontWeightInteger :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), Maybe CssDeclValue)
declValueAsFontWeightInteger (parser, token@(CssTokI i)) property = if i >= 100 && i <= 900
                                                                    then ((parser, token), Just defaultValue{typeTag = cssDeclValueTypeFontWeight, intVal = i})
                                                                    else ((parser, token), Nothing)
declValueAsFontWeightInteger (parser, token) property             = ((parser, token), Nothing)




tokenMatchesProperty :: CssToken -> Int -> Maybe Int
tokenMatchesProperty token property = tokenMatchesProperty' token acceptedValueTypes enums
  where
    propInfo = cssPropertyInfo V.! property
    acceptedValueTypes = tripletSnd propInfo
    enums = tripletThrd propInfo

    tokenMatchesProperty' :: CssToken -> [Int] -> [T.Text] -> Maybe Int
    tokenMatchesProperty' token (t:ts) enums | t == cssDeclValueTypeEnum =
                                                 case token of -- TODO: compare with similar code in in declValueAsEnum
                                                   CssTokSym symbol -> case L.elemIndex symbol enums of -- TODO: this search should be case-insensitive
                                                                         Just pos -> Just t
                                                                         Nothing  -> tokenMatchesProperty' token ts enums
                                                   _                -> tokenMatchesProperty' token ts enums

                                             | t == cssDeclValueTypeMultiEnum =
                                                 case token of
                                                   CssTokSym symbol -> if symbol == "none"
                                                                       then Just t
                                                                       else case L.elemIndex symbol enums of -- TODO: this search should be case-insensitive
                                                                              Just pos -> Just t
                                                                              Nothing  -> tokenMatchesProperty' token ts enums
                                                   _                -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeBgPosition =
                                                 case token of
                                                   CssTokSym s -> if s == "center" || s == "left" || s == "right" || s == "top" || s == "bottom"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   CssTokI i   -> Just t   -- TODO: here we should better handle numeric background positions
                                                   CssTokF f   -> Just t
                                                   _           -> tokenMatchesProperty' token ts enums

                                             | t == cssDeclValueTypeLengthPercent || t == cssDeclValueTypeLength || t == cssDeclValueTypeLengthPercentNumber =
                                                 case token of
                                                   CssTokF f -> if f < 0
                                                                then Nothing
                                                                else Just t
                                                   CssTokI i -> if i < 0
                                                                then Nothing
                                                                else Just t
                                                   _         -> Nothing

                                             | t == cssDeclValueTypeSignedLength =
                                                 case token of
                                                   CssTokF _ -> Just t
                                                   CssTokI _ -> Just t
                                                   _         -> tokenMatchesProperty' token ts enums

                                             | t == cssDeclValueTypeAuto =
                                                 case token of
                                                   CssTokSym symbol -> if symbol == "auto"
                                                                       then Just t
                                                                       else tokenMatchesProperty' token ts enums
                                                   _                 -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeColor =
                                                 case token of
                                                   CssTokCol c -> Just t -- We already know that the token is a valid color token
                                                   CssTokSym s -> case colorsStringToColor s of
                                                                    Just i -> Just t
                                                                    _      -> if s == "rgb"
                                                                              then Just t
                                                                              else tokenMatchesProperty' token ts enums
                                                   _           -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeString =
                                                 case token of
                                                   CssTokStr s -> Just t
                                                   _           -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeSymbol =
                                                 case token of
                                                   CssTokSym sym -> Just t
                                                   CssTokStr str -> Just t
                                                   _             -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeFontWeight =
                                                 case token of
                                                   CssTokI i -> if i >= 100 && i <= 900 -- TODO: this test of range is repeated in this file
                                                                then Just t
                                                                else tokenMatchesProperty' token ts enums
                                                   _         -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeURI =
                                                 case token of
                                                   CssTokSym s -> if s == "url"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   _           -> tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeInt = tokenMatchesProperty' token ts enums
                                             | t == cssDeclValueTypeUnused = tokenMatchesProperty' token ts enums
                                             | otherwise = Nothing

    tokenMatchesProperty' token [] _ = Nothing





{-
  Lengths are represented as int in the following way:

     | <------   integer value   ------> |

     +---+ - - - +---+---+- - - - - -+---+---+---+---+
     |          integer part             |   type    |
     +---+ - - - +---+---+- - - - - -+---+---+---+---+
     | integer part  | decimal fraction  |   type    |
     +---+ - - - +---+---+- - - - - -+---+---+---+---+
      n-1          15  14              3   2  1   0

     | <------ fixed point value ------> |

  where type is one of the CSS_LENGTH_TYPE_* values.
  CSS_LENGTH_TYPE_PX values are stored as
  29 bit signed integer, all other types as fixed point values.

What you see below is some wild attempt to make Haskell code correctly
interpret floats encoded in upper bits of integers. Not the best approach to
take.
-}


cssLengthType :: Int -> Int
cssLengthType length = length .&. 7




cssLengthValue :: Int -> Float
cssLengthValue len | t == cssLengthTypePX = let
                       z = (len `shiftR` 3)
                       in
                         if (0xf0000000 .&. len) == 0xf0000000
                         then fromIntegral ((-1) * ((4294967295 - len) `shiftR` 3) - 1)
                         else fromIntegral z
                   | t == cssLengthTypeNone
                     || t == cssLengthTypeMM
                     || t == cssLengthTypeEM
                     || t == cssLengthTypeEX
                     || t == cssLengthTypePercentage
                     || t == cssLengthTypeRelative =
                     (fromIntegral (up2 len)) / (fromIntegral down2)
                   | t == cssLengthTypeAuto = 0.0
                   | otherwise = 0.0
  where
    t = cssLengthType len
    up2 lenA = let
      z = lenA .&. (complement 0x00000007) :: Int
      in
        if (0xf0000000 .&. z) == 0xf0000000
        then (-1) * (4294967295 - z - 1)
        else z
    down2 = 1 `shiftL` 15 :: Int





css_LENGTH_FRAC_MAX = (1 `shiftL` (32 - 15 - 1)) - 1 :: Int
css_LENGTH_INT_MAX  = (1 `shiftL` (32 - 4)) - 1 :: Int

cssCreateLength :: Float -> Int -> Int
cssCreateLength val t | t == cssLengthTypePX = ((asInt1 (round (val))) `shiftL` 3) .|. t
                      | t == cssLengthTypeNone
                        || t == cssLengthTypeMM
                        || t == cssLengthTypeEM
                        || t == cssLengthTypeEX
                        || t == cssLengthTypePercentage
                        || t == cssLengthTypeRelative = ((round ((asInt2 val) * (fromIntegral shift15L))) .&. (complement 7)) .|. t

                      | t == cssLengthTypeAuto = t
                      | otherwise = cssLengthTypeAuto

  where
    shift15L = (1 `shiftL` 15) :: Int

    asInt1 :: Int -> Int
    asInt1 f = if f > css_LENGTH_INT_MAX
               then css_LENGTH_INT_MAX
               else if f < (-css_LENGTH_INT_MAX)
                    then (-css_LENGTH_INT_MAX)
                    else f

    asInt2 :: Float -> Float
    asInt2 f = if f > fromIntegral css_LENGTH_FRAC_MAX
               then fromIntegral css_LENGTH_FRAC_MAX
               else if f < fromIntegral (-css_LENGTH_FRAC_MAX)
                    then fromIntegral (-css_LENGTH_FRAC_MAX)
                    else f





ignoreBlock :: CssParser -> (CssParser, CssToken)
ignoreBlock parser = ignoreBlock' (parser, CssTokNone) 0
  where
    ignoreBlock' (parser, tok@CssTokEnd) depth    = (parser, tok)
    ignoreBlock' (parser, tok@(CssTokCh c)) depth | c == '{' = ignoreBlock' (nextToken parser) (depth + 1)
                                                  | c == '}' = if depth == 1
                                                              then nextToken parser
                                                              else ignoreBlock' (nextToken parser) (depth - 1)
                                                  | otherwise = ignoreBlock' (nextToken parser) depth
    ignoreBlock' (parser, tok) depth              = ignoreBlock' (nextToken parser) depth
{-
   while (tokenizer->type != CSS_TOKEN_TYPE_END) {
      if (tokenizer->type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer->value[0] == '{') {
            depth++;
         } else if (tokenizer->value[0] == '}') {
            depth--;
            if (depth == 0) {
               nextToken(tokenizer, hll_css_parser);
               return;
            }
         }
      }
      nextToken(tokenizer, hll_css_parser);
   }
-}




ignoreStatement :: CssParser -> (CssParser, CssToken)
ignoreStatement parser = ignoreStatement' (parser, CssTokNone)
  where
    ignoreStatement' (parser, tok@CssTokEnd)    = (parser, tok)
    ignoreStatement' (parser, tok@(CssTokCh c)) | c == ';' = nextToken parser
                                                | c == '{' = ignoreBlock parser
                                                | otherwise = ignoreStatement' (nextToken parser)
    ignoreStatement' (parser, tok)              = ignoreStatement' (nextToken parser)
{-
   while (tokenizer->type != CSS_TOKEN_TYPE_END) {
      if (tokenizer->type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer->value[0] == ';') {
            nextToken(tokenizer, hll_css_parser);
            return;
         } else if (tokenizer->value[0] =='{') {
            ignoreBlock(tokenizer, hll_css_parser);
            return;
         }
      }
      nextToken(tokenizer, hll_css_parser);
   }
-}




-- TODO: case-insensitive search
cssPropertyInfoIdxByName :: T.Text -> Maybe Int
cssPropertyInfoIdxByName propertyName = V.findIndex p cssPropertyInfo
  where
    p :: (T.Text, [Int], [T.Text]) -> Bool
    p = (\t -> (tripletFst t) == propertyName)




cssPropertyNameString :: Int -> T.Text
cssPropertyNameString property = tripletFst (cssPropertyInfo V.! property) -- TODO: no bounds checking?




cssParseWeight :: (CssParser, CssToken) -> ((CssParser, CssToken), Bool)
cssParseWeight (parser, CssTokCh '!') = case nextToken parser of
                                          (newParser, CssTokSym "important") -> (nextToken newParser, True)
                                          (newParser, tok)                   -> ((newParser, tok), False)
cssParseWeight (parser, tok)          = ((parser, tok), False)




data CssSimpleSelector = CssSimpleSelector {
    selectorPseudoClass :: [T.Text]
  , selectorId          :: T.Text
  , selectorClass       :: [T.Text]
  , selectorElement     :: Int
  , combinator          :: Int
  } deriving (Show)




data CssSelector = CssSelector {
    matchCaseOffset      :: Int
  , simpleSelectorList   :: [CssSimpleSelector]
  } deriving (Show)




cssSimpleSelectorElementNone = (-1)
cssSimpleSelectorElementAny  = (-2)




-- TODO: convert to data.
cssSelectorCombinatorNone            = 0
cssSelectorCombinatorDescendant      = 1   -- ' '
cssSelectorCombinatorChild           = 2   -- '>'
cssSelectorCombinatorAdjacentSibling = 3   -- '+'




defaultSimpleSelector = CssSimpleSelector {
    selectorPseudoClass = []
  , selectorId          = ""
  , selectorClass       = []
  , selectorElement     = cssSimpleSelectorElementAny

  -- Combinator that combines this simple selector and the previous one. For
  -- a simple selector that is first on the list (or the only on the list),
  -- the combinator will be None.
  , combinator          = cssSelectorCombinatorNone
  }



defaultSelector = CssSelector {
    matchCaseOffset    = -1
  , simpleSelectorList = [] -- [defaultSimpleSelector]
  }




data CssSelectorType =
    CssSelectorTypeNone
  | CssSelectorTypeClass
  | CssSelectorTypePseudoClass
  | CssSelectorTypeID




-- Update simple selector with given symbol 'sym', depending on type of
-- symbol.
updateSimpleSelector :: CssSimpleSelector -> CssSelectorType -> T.Text -> CssSimpleSelector
updateSimpleSelector simpleSelector selectorType sym =
  case selectorType of
    CssSelectorTypeClass       -> simpleSelector {selectorClass = (selectorClass simpleSelector) ++ [sym]}
    CssSelectorTypePseudoClass -> if T.null sym
                                  then simpleSelector
                                  else simpleSelector {selectorPseudoClass = (selectorPseudoClass simpleSelector) ++ [sym]}
    CssSelectorTypeID          -> if selectorId simpleSelector == ""
                                  then simpleSelector {selectorId = sym}
                                  else simpleSelector  -- TODO: is this valid that we ignore new value of the field without any warning?
    otherwise                  -> simpleSelector -- TODO: this probably should be caught by some kind of "non-exhaustive pattern match" warning.





--parseSelector (defaultParser{remainder="h1>h2+h3 h4 {something}"}, CssTokNone)
-- parseSelector (defaultParser{remainder="h1, h2, h3, h4, h5, h6, b, strong {font-weight: bolder}"}, CssTokNone)
-- parseSelector (defaultParser{remainder="address, article, aside, center, div, figure, figcaption, footer, h1, h2, h3, h4, h5, h6, header, nav, ol, p, pre, section, ul {display: block}i, em, cite, address, var"}, CssTokNone)




-- Create a selector from a group of tokens that is terminated by ',' or '{'
-- character.
--
-- Function always consumes the group of tokens, regardless of
-- success/failure of the parsing.
parseSelector :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssSelector)
parseSelector (parser, token) =
  case parseSelectorTokens (removeSpaceTokens selectorTokens []) [defaultSimpleSelector] of
    Just simpleSelectors -> ((newParser, newToken), Just defaultSelector{simpleSelectorList = reverse simpleSelectors})
    Nothing              -> ((newParser, newToken), Nothing)

  where
    ((newParser, newToken), selectorTokens) = takeSelectorTokens (parser, token)

    parseSelectorTokens :: [CssToken] -> [CssSimpleSelector] -> Maybe [CssSimpleSelector]
    parseSelectorTokens (CssTokSym sym:tokens) (simSel:simSels)  = parseSelectorTokens tokens ((simSel{selectorElement = htmlTagIndex sym}):simSels)

    parseSelectorTokens (CssTokCh '#':CssTokSym sym:tokens) (simSel:simSels) = parseSelectorTokens tokens ((updateSimpleSelector simSel CssSelectorTypeID sym):simSels)
    parseSelectorTokens (CssTokCh '.':CssTokSym sym:tokens) (simSel:simSels) = parseSelectorTokens tokens ((updateSimpleSelector simSel CssSelectorTypeClass sym):simSels)
    parseSelectorTokens (CssTokCh ':':CssTokSym sym:tokens) (simSel:simSels) = parseSelectorTokens tokens ((updateSimpleSelector simSel CssSelectorTypePseudoClass sym):simSels)

    parseSelectorTokens (CssTokCh '>':tokens) simSels = parseSelectorTokens tokens (defaultSimpleSelector{combinator = cssSelectorCombinatorChild}:simSels)
    parseSelectorTokens (CssTokCh '+':tokens) simSels = parseSelectorTokens tokens (defaultSimpleSelector{combinator = cssSelectorCombinatorAdjacentSibling}:simSels)
    parseSelectorTokens (CssTokWS:tokens)     simSels = parseSelectorTokens tokens (defaultSimpleSelector{combinator = cssSelectorCombinatorDescendant}:simSels)

    parseSelectorTokens [] simSels = Just simSels
    parseSelectorTokens _  simSels = Nothing




-- Take all tokens until ',' or '{' or EOF is met. The tokens will be used to
-- create list of CssSimpleSelectors (with separating combinators). If input
-- stream starts with whitespace, discard the whitespace (don't return token
-- for it) - leading whitespace is certainly meaningless.
takeSelectorTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeSelectorTokens (parser, token) = takeNext (parser, token) []
  where
    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (parser, token) tokens = case token of
                                        CssTokCh '{' -> ((parser, token), tokens)
                                        CssTokCh ',' -> ((parser, token), tokens)
                                        CssTokEnd    -> ((parser, token), tokens)
                                        -- Ignore whitespace occurring at the
                                        -- beginning of selectors list. I
                                        -- could filter it out later with
                                        -- removeSpaceTokens, but it's easier
                                        -- to not to add it at all.
                                        CssTokWS   -> if length tokens == 0
                                                      then takeNext (nextToken2 parser) tokens
                                                      else takeNext (nextToken2 parser) (tokens ++ [token])
                                        CssTokNone -> takeNext (nextToken2 parser) tokens -- This token can be used to 'kick-start' of parsing
                                        otherwise  -> takeNext (nextToken2 parser) (tokens ++ [token])





-- A dumb way to remove spaces that are adjactent to '+' or '>' combinators.
-- In such situations the spaces aren't combinators themselves but are just
-- separators.
removeSpaceTokens :: [CssToken] -> [CssToken] -> [CssToken]
removeSpaceTokens ((CssTokWS):(CssTokCh '+'):xs) acc = removeSpaceTokens ((CssTokCh '+'):xs) acc
removeSpaceTokens ((CssTokCh '+'):(CssTokWS):xs) acc = removeSpaceTokens ((CssTokCh '+'):xs) acc
removeSpaceTokens ((CssTokWS):(CssTokCh '>'):xs) acc = removeSpaceTokens ((CssTokCh '>'):xs) acc
removeSpaceTokens ((CssTokCh '>'):(CssTokWS):xs) acc = removeSpaceTokens ((CssTokCh '>'):xs) acc
removeSpaceTokens (x:(CssTokWS):[]) acc              = acc ++ [x] -- Don't forget to remove ending whitespace too.
removeSpaceTokens (x:xs) acc                         = removeSpaceTokens xs (acc ++ [x])
removeSpaceTokens [] acc                             = acc




data CssDeclValue = CssDeclValue {
    typeTag :: CssDeclValueType
  , intVal  :: Int
  , textVal :: T.Text
  , important :: Bool
  , property2 :: Int
  } deriving (Show)




-- Returned list inclues ':' character that is after the property name.
takePropertyTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takePropertyTokens (parser, token) = takeNext (parser, token) []
  where
    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (parser, token) tokens = case token of
                                        CssTokCh ':' -> (nextToken2 parser, (tokens ++ [token]))
                                        CssTokCh ';' -> ((parser, token), tokens)
                                        CssTokCh '}' -> ((parser, token), tokens)
                                        CssTokEnd    -> ((parser, token), tokens)
                                        CssTokNone -> takeNext (nextToken2 parser) tokens -- This token can be used to 'kick-start' of parsing
                                        otherwise  -> takeNext (nextToken2 parser) (tokens ++ [token])




defaultValue = CssDeclValue {
    typeTag = cssDeclValueTypeUnused
  , intVal  = 0
  , textVal = ""
  , important = False
  , property2 = (-1) -- TODO: somewhere there is a code that does not set property2 field.
  }




parseDeclProperty :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe Int)
parseDeclProperty (parser, token) = case property of
                                      Just _  -> ((newParser, newToken), property)
                                      Nothing ->   ((parser, token), Nothing)
  where
    ((newParser, newToken), propTokens) = takePropertyTokens (parser, token)
    property = case propTokens of
                 (CssTokSym sym:CssTokCh ':':[]) -> cssPropertyInfoIdxByName sym
                 otherwise                       -> Nothing



parseDeclValue2 :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), Maybe CssDeclValue, Int)
parseDeclValue2 (parser, token) property =
  case tokenMatchesProperty token property of
    Just valueType  -> case parseDeclValue (parser, token) valueType property of
                         ((p, t), Just v)  -> ((p, t), Just v{important = imp, property2=property}, property)
                         ((p, t), Nothing) -> ((p, t), Nothing, -2)
    Nothing -> ((parser, token), Nothing, -2)

  where
    imp = False -- TODO: cssParseWeight




parseDeclValue3 :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), [CssDeclValue])
parseDeclValue3 (parser, token) property =
  case tokenMatchesProperty token property of
    Just valueType  -> case parseDeclValue (parser, token) valueType property of
                         ((p, t), Just v)  -> ((p, t), [v{important = imp, property2=property}])
                         ((p, t), Nothing) -> ((p, t), [])
    Nothing -> ((parser, token), [])

  where
    imp = False -- TODO: cssParseWeight





parseDeclNormal :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclValue, Int)
parseDeclNormal (parser, token) = case parseDeclProperty (parser, token) of
                                    ((newParser, newToken), Just property) -> if property >= 0
                                                                              then parseDeclValue2 (newParser, newToken) property
                                                                              else ((parser, token), Nothing, -1)
                                    ((newParser, newToken), Nothing)       -> ((parser, token), Nothing, -1)




parseDeclValue :: (CssParser, CssToken) -> CssDeclValueType -> Int -> ((CssParser, CssToken), Maybe CssDeclValue)
parseDeclValue (parser, token) valueType property | valueType == cssDeclValueTypeInt                 = ((parser, token), Just defaultValue{typeTag = valueType, intVal = 0})
                                                  | valueType == cssDeclValueTypeEnum                = trace ("shorthand parseDeclValue enum " ++ (show token)) declValueAsEnum (parser, token) property
                                                  | valueType == cssDeclValueTypeMultiEnum           = trace ("shorthand parseDeclValue multi-enum " ++ (show token)) declValueAsMultiEnum (parser, token) property

                                                  | valueType == cssDeclValueTypeLengthPercent       = declValueAsLength (parser, token) valueType
                                                  | valueType == cssDeclValueTypeLength              = declValueAsLength (parser, token) valueType
                                                  | valueType == cssDeclValueTypeSignedLength        = declValueAsLength (parser, token) valueType
                                                  | valueType == cssDeclValueTypeLengthPercentNumber = declValueAsLength (parser, token) valueType

                                                  | valueType == cssDeclValueTypeAuto                = declValueAsAuto (parser, token)
                                                  | valueType == cssDeclValueTypeColor               = declValueAsColor (parser, token)
                                                  | valueType == cssDeclValueTypeFontWeight          = declValueAsFontWeightInteger (parser, token) property
                                                  | valueType == cssDeclValueTypeString              = trace ("shorthand parseDeclValue string " ++ (show token)) declValueAsString' (parser, token)
                                                  | valueType == cssDeclValueTypeSymbol              = trace ("shorthand parseDeclValue symbol " ++ (show token)) declValueAsSymbol (parser, token) ""
                                                  | valueType == cssDeclValueTypeURI                 = declValueAsURI (parser, token)
                                                  | valueType == cssDeclValueTypeBgPosition          = ((parser, token), Just defaultValue{typeTag = valueType, intVal = 12}) -- TODO
                                                  | valueType == cssDeclValueTypeUnused              = ((parser, token), Nothing)
                                                  | otherwise                                        = ((parser, token), Nothing)




-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
parseDeclarationMultiple :: (CssParser, CssToken) -> [Int] -> [CssDeclValue] -> ((CssParser, CssToken), [CssDeclValue])
parseDeclarationMultiple (parser, token) (prop:properties) values =
  case tokenMatchesProperty token prop of
    Just valueType -> case parseDeclValue (parser, token) valueType prop of
                        ((p, t), Just v)  -> parseDeclarationMultiple (p, t) properties (values ++ [v]) -- TODO: add setting 'important' member
                        ((p, t), Nothing) -> parseDeclarationMultiple (p, t) properties values
    Nothing        -> parseDeclarationMultiple (parser, token) properties values
parseDeclarationMultiple (parser, token) [] values                = ((parser, token), values)





parseDeclarationDirections :: (CssParser, CssToken) -> [Int] -> ((CssParser, CssToken), [CssDeclValue])
parseDeclarationDirections (parser, token) properties@(pt:pr:pb:pl:ps) = ((outParser, outToken), outVals)
  where outVals = case vals of
          (top:right:bottom:left:[]) -> [top{property2=pt}, right{property2=pr}, bottom{property2=pb}, left{property2=pl}]
          (top:rl:bottom:[])         -> [top{property2=pt}, rl   {property2=pr}, bottom{property2=pb}, rl  {property2=pl}]
          (tb:rl:[])                 -> [tb {property2=pt}, rl   {property2=pr}, tb    {property2=pb}, rl  {property2=pl}]
          (v:[])                     -> [v  {property2=pt}, v    {property2=pr}, v     {property2=pb}, v   {property2=pl}]
          []                         -> []
        ((outParser, outToken), vals) = matchOrderedTokens (parser, token) properties []
parseDeclarationDirections (parser, token) _ = ((parser, token), [])




-- Value tokens must be in proper order. Example: if property is
-- "border-color", and there are four value tokens, then tokens must
-- represent colors of "top","right","bottom","left" borders.
matchOrderedTokens :: (CssParser, CssToken) -> [Int] -> [CssDeclValue] -> ((CssParser, CssToken), [CssDeclValue])
matchOrderedTokens(parser, token) (prop:properties) values =
  case tokenMatchesProperty token prop of
    Just valueType -> case parseDeclValue (parser, token) valueType prop of
                        ((p, t), Just v)  -> matchOrderedTokens (p, t) properties (values ++ [v{property2=prop}]) -- TODO: add setting 'important' member
                        ((p, t), Nothing) -> ((p, t), values)
    Nothing        -> ((parser, token), values)
matchOrderedTokens (parser, token) [] values               = ((parser, token), values)





-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
parseDeclarationBorder :: (CssParser, CssToken) -> [Int] -> [CssDeclValue] -> ((CssParser, CssToken), [CssDeclValue])
parseDeclarationBorder (parser, token) (top:right:bottom:left:properties) values =
  case tokenMatchesProperty token top of
    Just valueType -> case parseDeclValue (parser, token) valueType top of
                        ((p, t), Just v)  -> parseDeclarationBorder (p, t) properties (values ++ [ v{property2=top}
                                                                                                 , v{property2=right}
                                                                                                 , v{property2=bottom}
                                                                                                 , v{property2=left}]) -- Add the same value for top/right/bottom/left border
                                                                                                               -- TODO: add setting 'important' member
                        ((p, t), Nothing) -> parseDeclarationBorder (p, t) properties values
    Nothing        -> parseDeclarationBorder (parser, token) properties values
parseDeclarationBorder (parser, token) [] values                                 = ((parser, token), values)




parseDeclarationShorthand :: (CssParser, CssToken) -> [Int] -> Int -> ((CssParser, CssToken), [CssDeclValue])
parseDeclarationShorthand (parser, token) properties shorthandType | shorthandType == cssShorthandTypeMultiple   = parseDeclarationMultiple (parser, token) properties []
                                                                   | shorthandType == cssShorthandTypeDirections = parseDeclarationDirections (parser, token) properties
                                                                   | shorthandType == cssShorthandTypeBorder     = parseDeclarationBorder (parser, token) properties []
                                                                   | shorthandType == cssShorthandTypeFont       = parseDeclarationMultiple (parser, token) properties []
                                                                   | otherwise = ((parser, token), [])




-- TODO: there is already a takePropertyTokens function.
takePropertyTokens2 :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takePropertyTokens2 (parser, token) =
  let (p2, t2) = nextToken parser
      (p3, t3) = nextToken p2
  in
    case (token, t2) of
      (CssTokSym _, CssTokCh ':') -> ((p3, t3), [token]) -- Don't return ':' token
      _                           -> ((parser, token), [])




parseDeclarationWrapper :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclValue])
parseDeclarationWrapper (parser, token) =
  case takePropertyTokens2 (parser, token) of
    ((p, t), [CssTokSym sym]) -> case cssPropertyInfoIdxByName sym of
                                   Just property -> if property >= 0
                                                    then parseDeclValue3 (p, t) property
                                                    else ((p, t), [])
                                   Nothing -> case cssShorthandInfoIdxByName sym of
                                     Just shorthandIdx -> if shorthandIdx >= 0
                                                          then parseDeclarationShorthand (p, t) properties shorthandType
                                                          else ((p, t), [])
                                       where properties = tripletThrd $ cssShorthandInfo V.! shorthandIdx
                                             shorthandType = tripletSnd $ cssShorthandInfo V.! shorthandIdx
                                     Nothing -> ((p, t), [])
    ((p, t), _)               -> ((p, t), [])



