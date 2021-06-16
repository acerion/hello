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

TODO: think about performance of using isPrefixOf to get just one character.
-}




{-# LANGUAGE OverloadedStrings #-}




module CssParser(nextToken
                , ignoreBlock
                , ignoreStatement
                , takeSymbol
                , takeInt

                , takeIdentToken
                , takeHashToken

                , cssPropertyInfo

                , parseUrl
                , CssParser (..)
                , CssToken (..)
                , CssNum (..)
                , tryTakingRgbFunction
                , parseRgbFunction
                , parseRgbFunctionInt

                , tokensAsValueColor
                , declValueAsString
                , tokensAsValueEnum
                , tokensAsValueMultiEnum
                , tokensAsValueAuto
                , tokensAsValueStringList
                , tokensAsValueBgPosition
                , tokensAsValueString
                , declValueAsFontWeightInteger
                , declValueAsLength
                , declValueAsURI

                , cssLengthTypeAuto

                , takeBgTokens

                , cssShorthandInfoIdxByName
                , cssPropertyInfoIdxByName
                , cssPropertyNameString

                , cssLengthType
                , cssLengthValue
                , cssCreateLength

                , parseDeclarationMultiple
                , parseDeclarationDirections
                , parseDeclarationBorder
                , parseDeclarationShorthand

                , cssShorthandTypeMultiple
                , cssShorthandTypeDirections
                , cssShorthandTypeBorder
                , cssShorthandTypeFont

                , takeLengthTokens

                , defaultValue
                , defaultSimpleSelector
                , defaultSelector
                , CssSimpleSelector (..)
                , CssSelector (..)
                , takeSelectorTokens
                , parseSelector
                , parseSelectors
                , removeSpaceTokens

                , CssValue (..)
                , CssValueType (..)
                , CssDeclaration (..)
                , parseDeclaration
                , takePropertyTokens

                , readName

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
import qualified Data.Map as M
import Data.Bits
import Colors
import HelloUtils
import HtmlTag
import Debug.Trace




data CssNum
    = CssNumI Int
    | CssNumF Float
    deriving (Show, Eq)

cssNumToFloat (CssNumF f) = f
cssNumToFloat (CssNumI i) = fromIntegral i




-- Tokens listed in https://www.w3.org/TR/css-syntax-3/#tokenization, but not
-- included in CssToken type (or not commented yet):
--
-- <function-token>, <at-keyword-token>, ,
-- <string-token>, <bad-string-token>, <url-token>, <bad-url-token>,
-- <delim-token>, <whitespace-token>, <CDO-token>, <CDC-token>,
-- <colon-token>, <semicolon-token>, <comma-token>, <[-token>, <]-token>,
-- <(-token>, <)-token>, <{-token>, and <}-token>.


data CssToken =
    CssTokNum CssNum            -- <number-token>
  | CssTokPerc CssNum           -- <percentage-token>
  | CssTokDim CssNum T.Text     -- <dimension-token>
  | CssTokIdent T.Text          -- <ident-token>; CSS3 spec says that text can be empty: "have a value composed of zero or more code points".
  | CssTokHash T.Text           -- <hash-token>; T.Text value is not prefixed by '#'.
  | CssTokSym T.Text
  | CssTokStr T.Text
  | CssTokCh Char
  | CssTokWS          -- Whitespace
  | CssTokEnd         -- End of input. No new tokens will appear in input.
  | CssTokNone        -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show, Eq)




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




data CssValueType =
    CssValueTypeInt                 -- This type is only used internally, for x-* properties.
  | CssValueTypeEnum                -- Value is i, if represented by enum_symbols[i].
  | CssValueTypeMultiEnum           -- For all enum_symbols[i], 1 << i are combined.
  | CssValueTypeLengthPercent       -- <length> or <percentage>. Represented by CssLength.
  | CssValueTypeLength              -- <length>, represented as CssLength.
                                    -- Note: In some cases, CSS_TYPE_LENGTH
                                    -- is used instead of
                                    -- CSS_TYPE_LENGTH_PERCENTAGE, only
                                    -- because Dw cannot handle percentages
                                    -- in this particular case (e.g.
                                    -- 'margin-*-width').
  | CssValueTypeSignedLength        -- As CSS_TYPE_LENGTH but may be negative.
  | CssValueTypeLengthPercentNumber -- <length> or <percentage>, or <number>
  | CssValueTypeAuto                -- Represented as CssLength of type cssLengthTypeAuto
  | CssValueTypeColor               -- Represented as integer.
  | CssValueTypeFontWeight          -- This very special and only used by 'font-weight'
  | CssValueTypeString              -- <string>
  | CssValueTypeStringList          -- List of symbols, which are directly
                                    -- copied (as opposed to
                                    -- CSS_PROPERTY_DATA_TYPE_ENUM and
                                    -- CSS_PROPERTY_DATA_TYPE_MULTI_ENUM).
                                    -- Used for 'font-family'. TODO: this
                                    -- should really be a Haskell list. No
                                    -- need to deconstruct the string to a
                                    -- list later.
  | CssValueTypeURI                 -- <uri>
  | CssValueTypeBgPosition          --
  | CssValueTypeUnused              -- Not yet used. Will itself get unused some day.
  deriving (Show, Eq)




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




-- Items with empty list of functions are not supported by this implementation.
cssPropertyInfo = V.fromList [
     ("background-attachment",  [ tokensAsValueEnum ],                                                css_background_attachment_enum_vals)
   , ("background-color",       [ tokensAsValueColor ],                                               [])
   , ("background-image",       [ declValueAsURI CssValueTypeURI ],                                              [])
   , ("background-position",    [ tokensAsValueBgPosition ],                                          [])
   , ("background-repeat",      [ tokensAsValueEnum ],                                                css_background_repeat_enum_vals)
   , ("border-bottom-color",    [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-bottom-style",    [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-bottom-width",    [ tokensAsValueEnum, declValueAsLength CssValueTypeLength ],          css_border_width_enum_vals)
   , ("border-collapse",        [ tokensAsValueEnum ],                                                css_border_collapse_enum_vals)
   , ("border-left-color",      [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-left-style",      [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-left-width",      [ tokensAsValueEnum, declValueAsLength  CssValueTypeLength ],         css_border_width_enum_vals)
   , ("border-right-color",     [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-right-style",     [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-rigth-width",     [ tokensAsValueEnum, declValueAsLength CssValueTypeLength ],          css_border_width_enum_vals)
   , ("border-spacing",         [ declValueAsLength CssValueTypeLength ],                             [])
   , ("border-top-color",       [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-top-style",       [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-top-width",       [ tokensAsValueEnum, declValueAsLength CssValueTypeLength ],          css_border_width_enum_vals)
   , ("bottom",                 [],                                                                   [])
   , ("caption-side",           [],                                                                   [])
   , ("clear",                  [],                                                                   [])
   , ("clip",                   [],                                                                   [])
   , ("color",                  [ tokensAsValueColor ],                                               [])
   , ("content",                [ tokensAsValueString ],                                              [])
   , ("counter-increment",      [],                                                                   [])
   , ("counter-reset",          [],                                                                   [])
   , ("cursor",                 [ tokensAsValueEnum ],                                                css_cursor_enum_vals)
   , ("direction",              [],                                                                   [])
   , ("display",                [ tokensAsValueEnum ],                                                css_display_enum_vals)
   , ("empty-cells",            [],                                                                   [])
   , ("float",                  [],                                                                   [])
   , ("font-family",            [ tokensAsValueStringList ],                                          [])
   , ("font-size",              [ tokensAsValueEnum, declValueAsLength CssValueTypeLengthPercent ],   css_font_size_enum_vals)
   , ("font-size-adjust",       [],                                                                   [])
   , ("font-stretch",           [],                                                                   [])
   , ("font-style",             [ tokensAsValueEnum ],                                                css_font_style_enum_vals)
   , ("font-variant",           [ tokensAsValueEnum ],                                                css_font_variant_enum_vals)
   , ("font-weight",            [ tokensAsValueEnum, declValueAsFontWeightInteger CssValueTypeFontWeight ],                 css_font_weight_enum_vals)
   , ("height",                 [ declValueAsLength CssValueTypeLengthPercent, tokensAsValueAuto ],   [])
   , ("left",                   [],                                                                   [])
   , ("letter-spacing",         [ tokensAsValueEnum, declValueAsSignedLength CssValueTypeSignedLength ],  css_letter_spacing_enum_vals)
   , ("line-height",            [ tokensAsValueEnum, declValueAsLength CssValueTypeLengthPercentNumber ], css_line_height_enum_vals)
   , ("list-style-image",       [],                                                                   [])
   , ("list-style-position",    [ tokensAsValueEnum ],                                                css_list_style_position_enum_vals)
   , ("list-style-type",        [ tokensAsValueEnum ],                                                css_list_style_type_enum_vals)
   , ("margin-bottom",          [ declValueAsSignedLength CssValueTypeSignedLength, tokensAsValueAuto ],[])
   , ("margin-left",            [ declValueAsSignedLength CssValueTypeSignedLength, tokensAsValueAuto ],[])
   , ("margin-right",           [ declValueAsSignedLength CssValueTypeSignedLength, tokensAsValueAuto ],[])
   , ("margin-top",             [ declValueAsSignedLength CssValueTypeSignedLength, tokensAsValueAuto ],[])
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
   , ("padding-bottom",         [ declValueAsLength CssValueTypeLength ],                             [])
   , ("padding-left",           [ declValueAsLength CssValueTypeLength ],                             [])
   , ("padding-right",          [ declValueAsLength CssValueTypeLength ],                             [])
   , ("padding-top",            [ declValueAsLength CssValueTypeLength ],                             [])
   , ("position",               [],                                                                   [])
   , ("quotes",                 [],                                                                   [])
   , ("right",                  [],                                                                   [])
   , ("text-align",             [ tokensAsValueEnum ],                                                css_text_align_enum_vals)
   , ("text-decoration",        [ tokensAsValueMultiEnum ],                                           css_text_decoration_enum_vals)
   , ("text-indent",            [ declValueAsLength CssValueTypeLengthPercent ],                      [])
   , ("text-shadow",            [],                                                                   [])
   , ("text-transform",         [ tokensAsValueEnum ],                                                css_text_transform_enum_vals)
   , ("top",                    [],                                                                   [])
   , ("unicode-bidi",           [],                                                                   [])
   , ("vertical-align",         [ tokensAsValueEnum ],                                                css_vertical_align_vals)
   , ("visibility",             [],                                                                   [])
   , ("white-space",            [ tokensAsValueEnum ],                                                css_white_space_vals)
   , ("width",                  [ declValueAsLength CssValueTypeLengthPercent, tokensAsValueAuto ],     [])
   , ("word-spacing",           [ tokensAsValueEnum, declValueAsSignedLength CssValueTypeSignedLength ],               css_word_spacing_enum_vals)
   , ("z-index",                [],                                                                   [])

   -- These are extensions, for internal used, and never parsed.
   -- TODO: verify whether we need them.
   -- TODO: verify if we still need "last" property.
   , ("x-link",                 [ declValueAsInt CssValueTypeInt ],                                              [])
   , ("x-colspan",              [ declValueAsInt CssValueTypeInt ],                                              [])
   , ("x-rowspan",              [ declValueAsInt CssValueTypeInt ],                                              [])
   , ("last",                   [], [])
   ] :: V.Vector CssPropertyInfo




type CssPropertyValueFun = (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
type CssPropertyInfo = (T.Text, [CssPropertyValueFun], [T.Text])




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
    (updatedParser, token) = case nextToken' parser{spaceSeparated = False} of
                               (p, Just t)  -> (p, t)
                               (p, Nothing) -> (p, CssTokNone)
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)



nextToken2 :: CssParser -> (CssParser, CssToken)
nextToken2 parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = case nextToken2' parser{spaceSeparated = False} of
                               (p, Just t)  -> (p, t)
                               (p, Nothing) -> (p, CssTokNone)
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)




-- This function is based on function with the same name from Real World
-- Haskell, chapter 10.
--
-- These lines are most awesome piece of code that I've written so far, in
-- any project.
(>>?) :: (CssParser, Maybe CssToken) -> (CssParser -> (CssParser, Maybe CssToken)) -> (CssParser, Maybe CssToken)
(parser, Nothing) >>? f = f parser
pair@(parser, _)  >>? _ = pair





nextToken' :: CssParser -> (CssParser, Maybe CssToken)
nextToken' parser = takeLeadingWhite parser >>?
                    takeNumPercDimToken     >>?
                    takeSymbol              >>?
                    takeString              >>?
                    takeHashToken           >>?
                    takeCharToken




nextToken2' :: CssParser -> (CssParser, Maybe CssToken)
nextToken2' parser = takeLeadingWhite2 parser >>?
                     takeNumPercDimToken      >>?
                     takeSymbol               >>?
                     takeString               >>?
                     takeHashToken            >>?
                     takeCharToken




-- Symbol must not start with a digit, therefore we have to have some kind of
-- test at the beginning. TODO: can we do the test in a better way?
--
-- TODO: Original C code parsed symbols starting with '-' (such as
-- "-webkit-user-select") in a way that resulted in token without the leading
-- '-' (so the resulting token was "webkit-user-select"). Haskell code keeps
-- the leading '-' character.
--
-- TODO: the function uses T.head on a string that can be empty.
takeSymbol :: CssParser -> (CssParser, Maybe CssToken)
takeSymbol parser = if predNonNumeric . T.head . remainder $ parser
                    then (parserMoveBy parser tok, Just $ CssTokSym tok)
                    else (parser, Nothing)
  where tok = T.takeWhile pred (remainder parser)
        predNonNumeric = (\c -> D.C.isAlpha c || c == '_' || c == '-')
        pred = (\c -> D.C.isAlphaNum c || c == '_' || c == '-')




-- Take <ident-token> from a string.
--
-- This implementation is not very pretty. It closely resembles algorithm
-- rescribed in CSS3 spec.
takeIdentToken :: CssParser -> (CssParser, Maybe CssToken)
takeIdentToken parser = if isValidStartOfIdentifier . remainder $ parser
                        then (parserMoveBy parser ident, Just $ CssTokIdent ident)
                        else (parser, Nothing)
  where
    (ident, n) = readName (remainder parser) "" 0




-- https://www.w3.org/TR/css-syntax-3/#check-if-three-code-points-would-start-an-identifier
isValidStartOfIdentifier buffer = case T.uncons buffer of
                                    Just (c, rem) | c == '-'               -> False -- TODO: properly handle identifier starting with '-'
                                                  | isNameStartCodePoint c -> True
                                                  | c == '\\'              -> False -- TODO: properly handle an escape
                                                  | otherwise              -> False
                                    Nothing -> False




isNameStartCodePoint :: Char -> Bool
isNameStartCodePoint c = (D.C.isAlpha c && D.C.isAscii c) || isNonAscii c || c == '_'


-- https://www.w3.org/TR/css-syntax-3/#non-ascii-code-point
isNonAscii :: Char -> Bool
isNonAscii c = D.C.ord c >= 0x80


isNameCodePoint :: Char -> Bool
isNameCodePoint c = isNameStartCodePoint c || D.C.isDigit c || c == '-'




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
takeString :: CssParser -> (CssParser, Maybe CssToken)
takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> (parser, Nothing)
  where
    parseString :: CssParser -> T.Text -> T.Text -> (CssParser, Maybe CssToken)
    parseString parser string rem = (parser{remainder = rem}, Just $ CssTokStr (escapedString string))
    escapedString str = case T.findIndex (== '\\') str of
                          Just i -> ""
                          Nothing -> str




-- TODO: the function probably should return <delim-token> in some situations.
-- TODO: what if there are no characters after '#'?
--
-- TODO: do we still need to use inBlock here? After the function has been
-- changed from takeColor to takeHashToken, it could be used to take ID
-- selector tokens. The function can be now very well used outside of block.
takeHashToken :: CssParser -> (CssParser, Maybe CssToken)
takeHashToken parser = if not $ inBlock parser
                       then (parser, Nothing) -- Don't take the leading '#' if we are not in a block;
                       else
                         case T.uncons $ remainder parser of
                           Just ('#', rem) -> (parser { remainder = T.drop (n + 1) $ remainder parser}, Just $ CssTokHash value)
                             -- TODO: That +1 for '#' above doesn't seem too clean. What if there are no valid characters after '#'?
                             where
                               (value, n) = readName rem "" 0
                           Just (c, rem)   -> (parser, Nothing)
                           Nothing         -> (parser, Nothing)




-- https://www.w3.org/TR/css-syntax-3/#consume-a-name
--
-- Returns pair (name, count), where count is a number of consumed code
-- points.
readName :: T.Text -> T.Text -> Int -> (T.Text, Int)
readName buffer acc n = case T.uncons buffer of
                          Just (c, rem) | isNameCodePoint c -> readName rem (T.snoc acc c) (n + 1)
                                        | c == '\\'         -> (acc, n) -- TODO: properly handle an escape
                                        | otherwise         -> (acc, n)
                          Nothing -> (acc, n)




takeCharToken :: CssParser -> (CssParser, Maybe CssToken)
takeCharToken parser = if T.null . remainder $ parser
                       then (parser, Nothing)
                       else (parserMoveBy parser (T.singleton . T.head . remainder $ parser),
                             Just $ CssTokCh (T.head . remainder $ parser))




-- This function does not return a token. Discarding meaningless data from
-- beginning of text would not create a valid token.
takeLeadingWhite :: CssParser -> (CssParser, Maybe CssToken)
takeLeadingWhite parser
  | T.null rem                 = (parser, Just CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = (parser, Nothing)
  where rem = remainder parser




-- This function may complete withouth returning a valid token. Discarding
-- meaningless data from beginning of text would not create a valid token.
takeLeadingWhite2 :: CssParser -> (CssParser, Maybe CssToken)
takeLeadingWhite2 parser
  | T.null rem                 = (parser, Just CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite2 parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite2 parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite2 parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = if (not . inBlock $ parser) && spaceSeparated parser
                                 then (parser, Just CssTokWS)
                                 else (parser, Nothing)
  where rem = remainder parser




-- Move parser's remainder by length of given string. Call this function when
-- givne string has been consumed to token and now you want to remove it from
-- front of parser's remainder.
parserMoveBy :: CssParser -> T.Text -> CssParser
parserMoveBy parser tok = parser { remainder = T.drop (T.length tok) (remainder parser) }




-- Try to interpret what comes after a <number-token> as <percentage-token>
-- or <dimension-token>.
tryTakingPercOrDim :: CssParser -> CssNum -> (CssParser, Maybe CssToken)
tryTakingPercOrDim numParser cssNum | (parser, Just (CssTokCh '%'))      <- takeCharToken numParser  = (parser, Just $ CssTokPerc cssNum)
                                    | (parser, Just (CssTokIdent ident)) <- takeIdentToken numParser = (parser, Just $ CssTokDim cssNum ident)
                                    | otherwise                                                      = (numParser, Nothing)




-- Take <number-token>, then try and see if what comes next in input string
-- allows to convert the <number-token> into <percentage-token> or
-- <dimension-token>. Return one of the three token types.
--
-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
takeNumPercDimToken :: CssParser -> (CssParser, Maybe CssToken)
takeNumPercDimToken parser | (numParser, Just cssNum) <- takeFloat parser = numTokenOrMore numParser cssNum
                           | (numParser, Just cssNum) <- takeInt parser   = numTokenOrMore numParser cssNum
                           | otherwise                                    = (parser, Nothing)

  where
    -- Use given CssNum to either create <number-token>, or (if data in
    -- parser allows it) to create <percentage-token> or <dimension-token>.
    numTokenOrMore :: CssParser -> CssNum -> (CssParser, Maybe CssToken)
    numTokenOrMore numParser cssNum = case tryTakingPercOrDim numParser cssNum of
                                        pair@(parser, Just token) -> pair
                                        -- Data in parser didn't allow creating other token, so just return <number-token>.
                                        (_, Nothing)              -> (numParser, Just $ CssTokNum cssNum)




-- Alternative implementation of takeNumPercDimToken. I like it less because
-- it *feels* like there is too much constructing compared to first version.
takeNumPercDimToken2 :: CssParser -> (CssParser, Maybe CssToken)
takeNumPercDimToken2 parser = (tupleParser, tupleToToken tuple)
  where
    -- A final <number/percentage/dimension-token> can be built from one or
    -- two tokens that are in a tuple.
    tupleToToken :: (Maybe CssToken, Maybe CssToken) -> Maybe CssToken
    tupleToToken (Just (CssTokNum cssNum), Nothing)                  = Just $ CssTokNum cssNum
    tupleToToken (Just (CssTokNum cssNum), Just (CssTokCh '%'))      = Just $ CssTokPerc cssNum
    tupleToToken (Just (CssTokNum cssNum), Just (CssTokIdent ident)) = Just $ CssTokDim cssNum ident
    tupleToToken _                                                   = Nothing

    -- Try taking Float before trying to take Int, because otherwise you may
    -- take only an initial (integral) part of Float as an Int, and leave
    -- fractional part in remainder.
    (tupleParser, tuple) | (numParser, Just cssNum) <- takeFloat parser = takeT2 numParser (Just $ CssTokNum cssNum, Nothing)
                         | (numParser, Just cssNum) <- takeInt parser   = takeT2 numParser (Just $ CssTokNum cssNum, Nothing)
                         | otherwise                                    = (parser, (Nothing, Nothing))

    takeT2 p1 (t1, _) | pair@(p2, Just (CssTokCh '%'))      <- takeCharToken p1  = (p2, (t1, snd pair))
                      | pair@(p2, Just (CssTokIdent ident)) <- takeIdentToken p1 = (p2, (t1, snd pair))
                      | otherwise                                                = (p1, (t1, Nothing))




-- TODO: this function doesn't recognize some float formats that are
-- valid in CSS, e.g. ".5".
takeFloat :: CssParser -> (CssParser, Maybe CssNum)
takeFloat parser = case T.R.signed T.R.rational (remainder parser) of
                     -- T.R.rational is happy to interpret "100" as
                     -- float, but we want to treat is as int and reject
                     -- it. Therefore we have to search for '.' in taken
                     -- sub-string :( TODO: what about "4e10" float
                     -- format that doesn't contain dot?
                     Right (f, rem) -> case T.find (== '.') val of
                                         Just c    -> (parser{remainder = rem}, Just $ CssNumF f)
                                         otherwise -> (parser, Nothing)
                       where
                         val = T.take valLen $ remainder parser
                         valLen = (T.length . remainder $ parser) - (T.length rem)
                     Left _         -> (parser, Nothing)




-- This function is very similar to takeFloat, but I don't want to write
-- a common function just yet. takeFloat will have to be updated to read
-- all formats of float value, and that change may make it more
-- complicated and less similar to takeInt.
takeInt :: CssParser -> (CssParser, Maybe CssNum)
takeInt parser = case T.R.signed T.R.decimal (remainder parser) of
                   Right (i, rem) -> (parser{remainder = rem}, Just $ CssNumI i)
                   Left _         -> (parser, Nothing)



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
  let (parser', tokens) = tryTakingRgbFunction $ parser
  in
    case reverse tokens of
      -- "either three integer values or three percentage values" in https://www.w3.org/TR/css-color-3/
      (CssTokCh '(':CssTokPerc (CssNumI r):CssTokCh ',':CssTokPerc (CssNumI g):CssTokCh ',':CssTokPerc (CssNumI b):CssTokCh ')':[]) -> (parser', Just (r, g, b, "%", True))
      (CssTokCh '(':CssTokNum (CssNumI r):CssTokCh ',':CssTokNum (CssNumI g):CssTokCh ',':CssTokNum (CssNumI b):CssTokCh ')':[])    -> (parser', Just (r, g, b, "/100", False))
      otherwise                                                                                                                     -> (parser', Nothing)




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




-- Interpret current token (and possibly more following tokens) as color
-- value (value of type CssValueTypeColor).
--
-- If current token is a Hash token, then there will be no need to take more
-- tokens. If current token is e.g. "rgb(" function, then the function should
-- (TODO) take as many tokens as necessary to build, parse and convert the
-- function into color value.
tokensAsValueColor :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueColor (parser, token@(CssTokHash str)) _ = case colorsHexStringToColor str of
                                                                    Just i  -> (nextToken parser, Just defaultValue{typeTag = CssValueTypeColor, intVal = i})
                                                                    Nothing -> (nextToken parser, Nothing)
tokensAsValueColor (parser, token@(CssTokSym s)) _ | s == "rgb" = case parseRgbFunctionInt parser of
                                                                              ((p, t), Just c)  -> ((p, t), Just defaultValue{typeTag = CssValueTypeColor, intVal = c})
                                                                              ((p, t), Nothing) -> ((p, t), Nothing)
                                                             | otherwise = case colorsStringToColor s of
                                                                             Just i  -> (nextToken parser, Just defaultValue{typeTag = CssValueTypeColor, intVal = i})
                                                                             Nothing -> (nextToken parser, Nothing)
tokensAsValueColor (parser, token) _               = ((parser, token), Nothing)





declValueAsString :: (CssParser, CssToken) -> CssPropertyInfo -> CssValueType -> ((CssParser, CssToken), Maybe T.Text)
declValueAsString (parser, token) propInfo valueType = case ((retParser, retToken), value) of
                                                         ((p, t), Just v)  -> ((p, t), Just (textVal v))
                                                         ((p, t), Nothing) -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | valueType == CssValueTypeString = tokensAsValueString (parser, token) []
                                   | valueType == CssValueTypeURI    = declValueAsURI valueType (parser, token) []
                                   | otherwise                       = ((parser, token), Nothing)




-- Interpret current token as enum value (value of type CssValueTypeEnum).
--
-- In case of enum value there is no need to consume more than current token
-- to build the Enum, but for consistency with other similar functions the
-- function is still called "tokensAs...".
tokensAsValueEnum :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueEnum (parser, token@(CssTokSym sym)) enums =
  case L.elemIndex (T.toLower sym) enums of -- TODO: should we use toLower when putting string in token or can we use it here?
    Just idx -> (nextToken parser, Just defaultValue{typeTag = CssValueTypeEnum, intVal = idx})
    Nothing  -> ((parser, token), Nothing)
  where
tokensAsValueEnum (parser, token) _                     = ((parser, token), Nothing)
                                                          -- TODO: is this the right place to reject everything else other than symbol?
                                                          -- Shouldn't we do it somewhere else?




-- Interpret current CssTokSym token (and possibly more following CssTokSym
-- tokens) as multi-enum value (value of type CssValueTypeEnum). Returned
-- integer is a bit vector with bits set for enums which were matched with
-- tokens.
--
-- In case of multi-enum value it's possible that consecutive CssTokSym
-- tokens after current CssTokSym token will be taken and perhaps matched
-- agains given enumeration of recognized values.
--
-- If input stream contains CssTokSym tokens with values not present in the
-- enumeration (perhaps they come from newer version of standard or perhaps
-- contain typos), then the function returns Nothing. Rationale: Firefox 78
-- and Chromium 90 don't apply this style:
-- "text-decoration: overline underline frog line-through;"
--
-- TODO: the function should be even stricter: the function should return
-- Nothing if any token in 'value' part of declaration *is not a CssTokSym
-- token*. In such case entired declaration should be rejected. This is
-- suggested by behaviour of FF and Chromium. Perhaps we should take a list
-- of tokens until end of value (until '}', ';' or EOF) and parse it as a
-- whole, to see if all value tokens are symbols/strings/identifiers.
--
-- TODO: if none of tokens match given list of enums then the function
-- doesn't consume any tokens and returns Nothing. I'm not entirely sure that
-- this is a good approach. Perhaps the function should return zero and
-- consume the tokens? But for consistency with other 'tokensAsValue*'
-- functions this function should return Nothing and don't consume any
-- tokens.
--
-- TODO: check in spec if the list of enums should always include an implicit
-- "none" value. Original C++ code indicates that "none" was treated in
-- special way.
tokensAsValueMultiEnum :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueMultiEnum pair@(_, CssTokSym sym) enums = case matchSymbolTokensWithListRigid pair enums 0x00 of
                                                         ((_, _), 0x00)  -> (pair, Nothing) -- None of input tokens were matched agains list of enums.
                                                         ((p2, t2), val) -> ((p2, t2), Just defaultValue{typeTag = CssValueTypeMultiEnum, intVal = val})
tokensAsValueMultiEnum (p, t) _                      = ((p, t), Nothing)




-- Match current CssTokSym token and any following CssTokSym tokens against
-- list of strings (enums). Each enum that had matching token is marked with
-- a bit set to '1' in bit vector.
--
-- 'Rigid' means that all values of tokens must be present in enumeration.
-- Any token not matching list of allowed values will result in returning
-- zero.
--
-- Return the bit vector.
--
-- TODO: write unit tests for this function if it ever gets used outside of
-- tokensAsValueMultiEnum. For now tests of tokensAsValueMultiEnum should be
-- enough, but if this function becomes more widely used, then it will
-- deserve its own tests set.
matchSymbolTokensWithListRigid :: (CssParser, CssToken) -> [T.Text] -> Int -> ((CssParser, CssToken), Int)
matchSymbolTokensWithListRigid (p, t@(CssTokSym sym)) enums bits =
  case L.elemIndex sym enums of -- TODO: should we use toLower when putting string in token or can we use it here?
    Just idx -> matchSymbolTokensWithListRigid (nextToken p) enums (bits .|. (1  `shiftL` idx))
    Nothing  -> ((p, t), 0x0) -- Given token does not match enumeration of allowed strings.
matchSymbolTokensWithListRigid (p, t) _ bits                   = ((p, t), bits)




-- Interpret current token as "auto" value (value of type CssValueTypeAuto).
--
-- In case of "auto" value there is no need to consume more than current
-- token to build the Auto, but for consistency with other similar functions
-- the function is still called "tokensAs...".
tokensAsValueAuto :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueAuto (p, t@(CssTokSym sym)) _ | T.toLower sym == "auto" = ((nextToken p), Just defaultValue{typeTag = CssValueTypeAuto, intVal = cssLengthTypeAuto})
                                           | otherwise               = ((p, t), Nothing)
tokensAsValueAuto (p, t) _                 = ((p, t), Nothing)




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
takeLengthTokens (parser, token) = case token of
                                     CssTokNum  _   -> (nextToken parser, [token])
                                     CssTokPerc  _  -> (nextToken parser, [token])
                                     CssTokDim  _ _ -> (nextToken parser, [token])
                                     CssTokCh ';'   -> ((parser, token), [])
                                     CssTokCh '}'   -> ((parser, token), [])
                                     CssTokEnd      -> ((parser, token), [])
                                     _              -> ((parser, token), [])

{-
  where
    numberWithSomething (parser, numberToken) = case nextToken parser of
                                                  pair@(p3, CssTokSym sym)  -> if unitStringIsValid sym
                                                                               then (nextToken p3, [numberToken, snd pair])
                                                                               else (nextToken p3, []) -- TODO: how to handle unrecognized symbol?
                                                  pair@(p3, CssTokPercI i) -> (nextToken p3, [numberToken, snd pair])
                                                  pair@(p3, CssTokPercF f) -> (nextToken p3, [numberToken, snd pair])
                                                  pair@(p3, CssTokCh ';')  -> (pair, [numberToken])
                                                  pair@(p3, CssTokCh '}')  -> (pair, [numberToken])
                                                  pair@(p3, CssTokEnd)     -> (pair, [numberToken])
                                                  pair                     -> ((parser, token), [])

    unitStringIsValid str = str == "px" || str == "mm" || str == "cm" || str == "in" || str == "pt" || str == "pc" || str == "em" || str == "ex"
-}




declValueAsSignedLength = declValueAsLength




declValueAsLength :: CssValueType -> (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsLength valueType (parser, token) enums =
  case tokens of
    [CssTokDim cssNum ident] -> ((newParser, newToken), unitValue valueType cssNum ident)
    [CssTokPerc cssNum]      -> ((newParser, newToken), percentValue valueType cssNum)
    [CssTokNum cssNum]       -> ((newParser, newToken), unitlessValue valueType cssNum)
    _                        -> ((parser, token), Nothing)
  where
    ((newParser, newToken), tokens) = takeLengthTokens (parser, token)

    percentValue :: CssValueType -> CssNum -> Maybe CssValue
    percentValue valueType cssNum = Just defaultValue{typeTag = valueType, intVal = (cssCreateLength val t)}
      where
        fval = cssNumToFloat cssNum
        (val, t) = ((fval / 100.0), cssLengthTypePercentage)

    unitValue :: CssValueType -> CssNum -> T.Text -> Maybe CssValue
    unitValue valueType cssNum unitString = Just defaultValue{typeTag = valueType, intVal = (cssCreateLength val t)}
      where
        fval = cssNumToFloat cssNum
        (val, t) = lengthValueToValAndType fval (T.toLower unitString)

    unitlessValue :: CssValueType -> CssNum -> Maybe CssValue
    -- Allow numbers without unit only for 0 orLengthPercentNumber. TODO: why?
    unitlessValue valueType cssNum = if (valueType == CssValueTypeLengthPercentNumber || fval == 0.0)
                                     then Just defaultValue{typeTag = valueType, intVal = (cssCreateLength val t)}
                                     else Nothing
      where
        fval = cssNumToFloat cssNum
        (val, t) = (fval, cssLengthTypeNone)




-- Interpret current token as "string" value (value of type CssValueTypeString).
--
-- In case of "string" value there is no need to consume more than current
-- token to build the String, but for consistency with other similar
-- functions the function is still called "tokensAs...".
tokensAsValueString :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueString (p, (CssTokStr s)) _ = (nextToken p, Just defaultValue{typeTag = CssValueTypeString, textVal = s})
tokensAsValueString (p, t) _             = ((p, t), Nothing)




declValueAsURI :: CssValueType -> (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsURI valueType (parser, token) enums = case parseUrl (parser, token) of
                                                   ((newParser, newToken), Just url) -> ((newParser, newToken), Just defaultValue{typeTag = valueType, textVal = url})
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




tokensAsValueBgPosition :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueBgPosition (parser, token) _ = ((outParser, outToken), value)
  where
    ((outParser, outToken), tokens) = takeBgTokens (parser, token)
    value = Just defaultValue{typeTag = CssValueTypeBgPosition, intVal = 12}
    -- TODO: right now the original dillo doesn't seem to display background
    -- images at all, so I will stop the work on this function for now.
    -- Later, as I get to know dillo better, I will resume work on this
    -- functionality. Look at "case CSS_TYPE_BACKGROUND_POSITION" in
    -- src/cssparser.cc in original dillo code.
    --
    -- This functionality will require adding posX/posY fields to CssValue.




takeBgTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeBgTokens (parser, token) = ((outParser, outToken), outTokens)

  where
    ((outParser, outToken), tokens) = takeBgTokens' (parser, token) []
    outTokens = remapToken <$> (reorderTokens tokens)

    -- Make sure that list of tokens always contains two tokens that are
    -- properly ordered: [horiz, vert].
    reorderTokens :: [CssToken] -> [CssToken]
    reorderTokens tokens@[CssTokSym "top", _]    = reverse tokens -- First token should be horiz, second should be vert.
    reorderTokens tokens@[CssTokSym "bottom", _] = reverse tokens -- First token should be horiz, second should be vert.
    reorderTokens tokens@[CssTokSym "initial"]   = tokens -- Handle single-element "initial" first, before other single-element lists.
    reorderTokens tokens@[CssTokSym "inherit"]   = tokens -- Handle single-element "inherit" first, before other single-element lists.
    -- After "initial" and "inherit" are handled, you can now add 50% as
    -- missing second element. Also call reorderTokens recursively to handle
    -- this input string correctly: "top;".
    -- You have to ensure two things for this case:
    -- 1. output list has two members: one of them is the "top" and the other
    --    is default "50%"), therefore we add "50%" token
    -- 2. horiz/vert tokens are in proper order (horiz first, vert second),
    --    therefore we do recursive call to reorderTokens.
    reorderTokens [tok1]                            = reorderTokens [tok1, CssTokPerc $ CssNumI 50]
    reorderTokens tokens@[tok1, tok2]               = tokens
    reorderTokens _                                 = [] -- TODO: Perhas this is not needed and we should trust that caller will pass non-empty list?


    -- Change CssTokSym tokens for top/left/center etc. into <percentage-token>s.
    -- TODO: this function doesn't handle "initial" and "inherit" - what do we do with them?
    remapToken :: CssToken -> CssToken
    remapToken tok@(CssTokSym sym) = case M.lookup sym posMap of
                                       Just percToken -> percToken
                                       Nothing        -> tok -- TODO: this will happen for "initial" and "inherit" tokens, which aren't really handled here.
      where posMap = M.fromList [ ("left",   CssTokPerc $ CssNumI 0)
                                , ("right",  CssTokPerc $ CssNumI 100)
                                , ("top",    CssTokPerc $ CssNumI 0)
                                , ("bottom", CssTokPerc $ CssNumI 100)
                                , ("center", CssTokPerc $ CssNumI 50) ]
    remapToken tok@(CssTokPerc cssNum)      = tok
    remapToken tok@(CssTokDim cssNum ident) = tok




takeBgTokens' :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
takeBgTokens' (parser, token) tokens = ((outParser, outToken), outTokens)

  where
    ((outParser, outToken), outTokens) = if doContinue tokens token
                                         then case token of
                                                CssTokNone -> takeBgTokens' (nextToken parser) tokens -- Take the token, but don't append it to result
                                                _          -> takeBgTokens' (nextToken parser) (tokens ++ [token])
                                         else if tokensValid tokens
                                              then ((parser, token), tokens)
                                              else ((parser, token), [])



    doContinue tokens token = length tokens < 2 && tokValid token

    tokValid (CssTokNone)             = True -- used to kick-start parsing of stream
    tokValid (CssTokSym sym)          = elem sym horizVals || elem sym vertVals || elem sym otherVals || sym == "center"
    tokValid (CssTokNum cssNum)       = True
    tokValid (CssTokDim cssNum ident) = True
    tokValid (CssTokPerc cssNum)      = True
    tokValid _                        = False

    horizVals = ["left", "right"]
    vertVals  = ["top", "bottom"]
    otherVals = ["initial", "inherit"]

    tokensValid [CssTokSym sym1, CssTokSym sym2] = cond1 && cond2 && cond3
      where
        cond1 = not (elem sym1 otherVals && elem sym2 otherVals) -- "initial" or "inherit" isn't used twice.
        cond2 = not (elem sym1 horizVals && elem sym2 horizVals) -- Both symbols aren't from the same list of horizontal tokens.
        cond3 = not (elem sym1 vertVals  && elem sym2 vertVals)  -- Both symbols aren't from the same list of vertical tokens.
    tokensValid [tok1, tok2] = True
    tokensValid [tok1]       = True -- Single-token list is valid: token's value will be used as X, and Y will be set to 50%.
    tokensValid _            = False




-- Interpret current CssTokSym/CssTokStr token (and possibly more following
-- CssTokSym and CssTokStr tokens) as list value (value of type
-- CssValueTypeStringList). The tokens should be separated by comma tokens.
-- Returned value is a string of items separated by commas.
--
-- TODO: how we should handle list separated by spaces instead of commas? How
-- should we handle multiple consecutive commas?
--
-- TODO: all tokens in declaration's value should be
-- strings/symbols/commas/spaces. There can be no other tokens (e.g. numeric
-- or hash). Such declaration should be rejected:
-- 'font-family: "URW Gothic L", "Courier New", monospace, 90mph'
-- Rationale: behaviour of FF and Chromium.
--
-- Read comma-separated list of items, e.g. font family names. The items can
-- be strings with spaces, therefore the function consumes both CssTokSym and
-- CssTokStr tokens. TODO: test the code for list of symbols separated by
-- space or comma.
tokensAsValueStringList :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueStringList (parser, token) enums = asList (parser, token) []
  where
    asList :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
    asList (p, (CssTokSym sym)) acc  = asList (nextToken p) (sym:acc)
    asList (p, (CssTokStr str)) acc  = asList (nextToken p) (str:acc)
    asList (p, (CssTokCh  ',')) acc  = asList (nextToken p) (",":acc)
    asList (p, t@(CssTokCh ';')) acc = final (p, t) acc
    asList (p, t@(CssTokCh '}')) acc = final (p, t) acc
    asList (p, t@(CssTokEnd)) acc    = final (p, t) acc
    asList (p, t) acc                = ((parser, token), Nothing) -- TODO: this implmentation does not allow for final "!important" token.

    final (p, t) acc = if 0 == length acc
                       then ((p, t), Nothing)
                       else ((p, t), Just defaultValue{ typeTag = CssValueTypeStringList
                                                      , textVal = T.concat . reverse $ acc})




{-
   case CssValueTypeSYMBOL:

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


declValueAsFontWeightInteger :: CssValueType -> (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsFontWeightInteger typeValue (parser, token@(CssTokNum (CssNumI i))) _ = if i >= 100 && i <= 900
                                                                                   then ((parser, token), Just defaultValue{typeTag = typeValue, intVal = i})
                                                                                   else ((parser, token), Nothing)
declValueAsFontWeightInteger _ (parser, token) _                                 = ((parser, token), Nothing)



{-
tokenMatchesProperty :: CssToken -> CssPropertyInfo -> Maybe CssValueType
tokenMatchesProperty token propInfo = tokenMatchesProperty' token acceptedValueTypes enums
  where
    acceptedValueTypes = tripletSnd propInfo
    enums = tripletThrd propInfo

    tokenMatchesProperty' :: CssToken -> [CssValueType] -> [T.Text] -> Maybe CssValueType
    tokenMatchesProperty' token (t:ts) enums
                                             | t == CssValueTypeBgPosition =
                                                 case token of
                                                   CssTokSym s -> if s == "center" || s == "left" || s == "right" || s == "top" || s == "bottom"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   CssTokNum n -> Just t   -- TODO: here we should better handle numeric background positions
                                                   _           -> tokenMatchesProperty' token ts enums

                                             | t == CssValueTypeLengthPercent || t == CssValueTypeLength || t == CssValueTypeLengthPercentNumber =
                                                 case token of
                                                   CssTokNum (CssNumF f)   -> if f < 0 then Nothing else Just t
                                                   CssTokNum (CssNumI i)   -> if i < 0 then Nothing else Just t
                                                   CssTokPerc (CssNumF f)  -> if f < 0 then Nothing else Just t
                                                   CssTokPerc (CssNumI i)  -> if i < 0 then Nothing else Just t
                                                   CssTokDim (CssNumF f) _ -> if f < 0 then Nothing else Just t
                                                   CssTokDim (CssNumI i) _ -> if i < 0 then Nothing else Just t
                                                   _              -> Nothing

                                             | t == CssValueTypeSignedLength =
                                                 case token of
                                                   CssTokNum _   -> Just t
                                                   CssTokPerc _  -> Just t
                                                   CssTokDim _ _ -> Just t
                                                   _             -> tokenMatchesProperty' token ts enums

                                             | t == CssValueTypeFontWeight =
                                                 case token of
                                                   CssTokNum (CssNumI i) -> if i >= 100 && i <= 900 -- TODO: this test of range is repeated in this file
                                                                            then Just t
                                                                            else tokenMatchesProperty' token ts enums
                                                   _                     -> tokenMatchesProperty' token ts enums
                                             | t == CssValueTypeURI =
                                                 case token of
                                                   CssTokSym s -> if s == "url"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   _           -> tokenMatchesProperty' token ts enums
-}




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
    p :: (T.Text, [CssPropertyValueFun], [T.Text]) -> Bool
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
parseSelector (parser, token) = ((outParser, outToken), selector)

  where
    (outParser, outToken) = consumeRestOfSelector (p2, t2)
    ((p2, t2), selector) = case parseSelectorTokens (removeSpaceTokens selectorTokens []) [defaultSimpleSelector] of
                             Just simpleSelectors -> ((newParser, newToken), Just defaultSelector{simpleSelectorList = reverse simpleSelectors})
                             Nothing              -> ((newParser, newToken), Nothing)


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




-- Parse entire list of selectors that are separated with comma.
--
-- Note from dillo:
--
-- TODO: dump whole ruleset in case of parse error as required by CSS 2.1
-- however make sure we don't dump it if only dillo fails to parse valid CSS.
parseSelectors :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssSelector])
parseSelectors (parser, token) = parseSelectorWrapper (parser, token) []
  where
    parseSelectorWrapper (parser, token) acc =
      case parseSelector (parser, token) of
        ((parser, token), Just selector) -> case token of
                                              CssTokCh ',' -> parseSelectorWrapper (nextToken parser) (acc ++ [selector])
                                              otherwise    -> ((parser, token), acc ++ [selector])
        _                                -> ((parser, token), acc)





-- Find end of current selector (probably needed only if something goes wrong
-- during parsign of current selector).
consumeRestOfSelector pair@(parser, CssTokEnd)    = pair
consumeRestOfSelector pair@(parser, CssTokCh '{') = pair
consumeRestOfSelector pair@(parser, CssTokCh ',') = pair
consumeRestOfSelector (parser, _)                 = consumeRestOfSelector . nextToken $ parser




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




data CssValue = CssValue {
    typeTag :: CssValueType
  , intVal  :: Int
  , textVal :: T.Text
  } deriving (Show, Eq)




defaultValue = CssValue {
    typeTag = CssValueTypeUnused
  , intVal  = 0
  , textVal = ""
  }




data CssDeclaration = CssDeclaration
  { property  :: Int
  , value     :: CssValue

  -- https://www.w3.org/TR/css-syntax-3
  --
  -- "If the last two non-<whitespace-token>s in the declaration’s value are
  -- a <delim-token> with the value "!" followed by an <ident-token> with a
  -- value that is an ASCII case-insensitive match for "important", remove
  -- them from the declaration’s value and set the declaration’s important
  -- flag to true."
  --
  -- So "important" is per-declaration flag.
  , important :: Bool
  } deriving (Show)




defaultDeclaration = CssDeclaration
  { property  = (-1) -- TODO: somewhere there is a code that does not set property2 field.
  , value     = defaultValue
  , important = False
  }




parseDeclarationNormal :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationNormal (parser, token) property =
  case parseDeclValue (parser, token) enums functions  of
    ((p, t), Just v)  -> ((p, t), [defaultDeclaration{property = property, value = v}])
    ((p, t), Nothing) -> ((p, t), [])
  where
    propInfo = (cssPropertyInfo V.! property)
    functions = tripletSnd propInfo
    enums = tripletThrd propInfo




parseDeclValue :: (CssParser, CssToken) -> [T.Text] -> [CssPropertyValueFun] -> ((CssParser, CssToken), Maybe CssValue)
parseDeclValue (parser, token) enums []     = ((parser, token), Nothing)
parseDeclValue (parser, token) enums (f:fs) = case f (parser, token) enums of
                                                all@((parser, token), Just value) -> all
                                                otherwise                         -> parseDeclValue (parser, token) enums fs




-- C++ code in dillo commented "Int" with "Not used for parser values."
-- CssValueTypeInt is used internally only.
-- TODO: clarify why we need this at all.
declValueAsInt valueType (parser, token) enums = ((parser, token), Just defaultValue{typeTag = valueType, intVal = 0})





-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
parseDeclarationMultiple :: (CssParser, CssToken) -> [Int] -> [CssDeclaration] -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationMultiple (parser, token) (prop:properties) ds =
  case parseDeclValue (parser, token) enums functions of
    ((p, t), Just v)  -> parseDeclarationMultiple (p, t) properties (ds ++ [defaultDeclaration{property = prop, value = v}])
    ((p, t), Nothing) -> parseDeclarationMultiple (p, t) properties ds
  where
    propInfo = (cssPropertyInfo V.! prop)
    functions = tripletSnd propInfo
    enums = tripletThrd propInfo
parseDeclarationMultiple (parser, token) [] ds                = ((parser, token), ds)




parseDeclarationDirections :: (CssParser, CssToken) -> [Int] -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationDirections (parser, token) properties@(pt:pr:pb:pl:ps) = ((outParser, outToken), ds)
  where ds = case vals of
          (top:right:bottom:left:[]) -> [ defaultDeclaration{property = pt, value = top}
                                        , defaultDeclaration{property = pr, value = right}
                                        , defaultDeclaration{property = pb, value = bottom}
                                        , defaultDeclaration{property = pl, value = left}]
          (top:rl:bottom:[])         -> [ defaultDeclaration{property = pt, value = top}
                                        , defaultDeclaration{property = pr, value = rl}
                                        , defaultDeclaration{property = pb, value = bottom}
                                        , defaultDeclaration{property = pl, value = rl}]
          (tb:rl:[])                 -> [ defaultDeclaration{property = pt, value = tb}
                                        , defaultDeclaration{property = pr, value = rl}
                                        , defaultDeclaration{property = pb, value = tb}
                                        , defaultDeclaration{property = pl, value = rl}]
          (v:[])                     -> [ defaultDeclaration{property = pt, value = v}
                                        , defaultDeclaration{property = pr, value = v}
                                        , defaultDeclaration{property = pb, value = v}
                                        , defaultDeclaration{property = pl, value = v}]
          []                         -> []
        ((outParser, outToken), vals) = matchOrderedTokens (parser, token) properties []
parseDeclarationDirections (parser, token) _ = ((parser, token), [])




-- Value tokens must be in proper order. Example: if property is
-- "border-color", and there are four value tokens, then tokens must
-- represent colors of "top","right","bottom","left" borders.
matchOrderedTokens :: (CssParser, CssToken) -> [Int] -> [CssValue] -> ((CssParser, CssToken), [CssValue])
matchOrderedTokens (parser, token) (prop:properties) values =
  case parseDeclValue (parser, token) enums functions of
    ((p, t), Just v)  -> matchOrderedTokens (p, t) properties (values ++ [v])
    ((p, t), Nothing) -> ((p, t), values)
  where
    propInfo = (cssPropertyInfo V.! prop)
    functions = tripletSnd propInfo
    enums = tripletThrd propInfo
matchOrderedTokens (parser, token) [] values               = ((parser, token), values)





-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
parseDeclarationBorder :: (CssParser, CssToken) -> [Int] -> [CssDeclaration] -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationBorder (parser, token) (top:right:bottom:left:properties) ds =
  case parseDeclValue (parser, token) enums functions  of
    ((p, t), Just v)  -> parseDeclarationBorder (p, t) properties (ds ++ [ defaultDeclaration{property = top,    value = v}
                                                                         , defaultDeclaration{property = right,  value = v}
                                                                         , defaultDeclaration{property = bottom, value = v}
                                                                         , defaultDeclaration{property = left,   value = v}])

    ((p, t), Nothing) -> parseDeclarationBorder (p, t) properties ds
  where
    propInfo = (cssPropertyInfo V.! top)
    functions = tripletSnd propInfo
    enums = tripletThrd propInfo
parseDeclarationBorder (parser, token) [] ds                                 = ((parser, token), ds)




parseDeclarationShorthand :: (CssParser, CssToken) -> [Int] -> Int -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationShorthand (parser, token) properties shorthandType | shorthandType == cssShorthandTypeMultiple   = parseDeclarationMultiple (parser, token) properties []
                                                                   | shorthandType == cssShorthandTypeDirections = parseDeclarationDirections (parser, token) properties
                                                                   | shorthandType == cssShorthandTypeBorder     = parseDeclarationBorder (parser, token) properties []
                                                                   | shorthandType == cssShorthandTypeFont       = parseDeclarationMultiple (parser, token) properties []
                                                                   | otherwise = ((parser, token), [])




takePropertyTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takePropertyTokens (parser, nameToken) =
  let (colonParser, colonToken) = nextToken parser
      (retParser, retToken) = nextToken colonParser
  in
    case (nameToken, colonToken) of
      (CssTokSym _, CssTokCh ':') -> ((retParser, retToken), [nameToken]) -- Don't return ':' token. Only 'property name' token is significant to caller.
      _                           -> ((parser, nameToken), [])




-- The function returns a list of declarations because a line in CSS with a
-- shorthand declaration will be translated in N corresponding "normal"
-- declarations. E.g. "border-color: red" shorthand will be translated into a
-- list of "normal" declarations that will look like this:
-- ["border-top-color: red"; "border-right-color: red"; "border-bottom-color: red"; "border-left-color: red"]
parseDeclarationWrapper :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationWrapper (parser, token) =
  case takePropertyTokens (parser, token) of
    ((p, t), [CssTokSym sym]) -> case cssPropertyInfoIdxByName sym of
                                   Just property -> tryNormal (p, t) property
                                   Nothing -> case cssShorthandInfoIdxByName sym of
                                     Just shorthandIdx -> tryShorthand (p, t) shorthandIdx
                                     Nothing -> ((p, t), [])
    ((p, t), _)               -> ((p, t), [])




tryNormal = parseDeclarationNormal




tryShorthand (parser, token) shorthandIdx = parseDeclarationShorthand (parser, token) properties shorthandType
  where
    properties = tripletThrd $ cssShorthandInfo V.! shorthandIdx
    shorthandType = tripletSnd $ cssShorthandInfo V.! shorthandIdx





-- For non-shorthand declaration, this function should produce one-element
-- list. But a shorthand declaration translates into two or more regular
-- declarations, hence the return type contains a list of declarations.
parseDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseDeclaration (parser, token) = ((outParser, outToken), declarations)
  -- TODO: add setting 'important' field of declarations here
  where
    ((p2, t2), declarations) = parseDeclarationWrapper (parser, token)
    (outParser, outToken) = consumeRestOfDeclaration (p2, t2)




-- Find end of current declaration (probably needed only if something goes
-- wrong during parsign of current declaration).
consumeRestOfDeclaration pair@(parser, CssTokEnd)    = pair
consumeRestOfDeclaration pair@(parser, CssTokCh '}') = pair -- '}' is not a part of declaration, so don't go past it. Return '}' as current token.
consumeRestOfDeclaration (parser, CssTokCh ';')      = nextToken parser
consumeRestOfDeclaration (parser, _)                 = consumeRestOfDeclaration . nextToken $ parser


