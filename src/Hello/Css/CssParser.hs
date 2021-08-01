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
{-# LANGUAGE ScopedTypeVariables #-}



module Hello.Css.Parser(nextToken
                       , ignoreBlock
                       , ignoreStatement
                       , takeInt

                       , takeIdentToken
                       , takeIdentLikeToken
                       , takeHashToken

                       , cssPropertyInfo

                       , cssSimpleSelectorElementAny
                       , cssSimpleSelectorElementNone

                       , parseUrl
                       , CssParser (..)
                       , CssToken (..)
                       , CssNum (..)

                       , consumeFunctionTokens
                       , interpretRgbFunctionTokens
                       , rgbFunctionToColor

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
                       , CssLength (..)

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
                       , readSelectorList
                       , removeSpaceTokens

                       , CssCombinator (..)

                       , CssValue (..)
                       , CssDeclaration (..)
                       , parseDeclaration
                       , parseDeclarationWrapper2
                       , takePropertyTokens
                       , defaultDeclaration
                       , parseElementStyleAttribute
                       , parseAllDeclarations

                       , declarationsSetUpdateOrAdd
                       , declarationsSetAppend
                       , CssDeclarationSet (..)
                       , defaultCssDeclarationSet

                       , CssRule (..)
                       , getTopSimSel
                       , getRequiredMatchCache

                       , consumeName

                       , CssOrigin (..)

                       , defaultParser)
  where




--import Prelude
import Data.Maybe
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.IO as T.IO
import qualified Hello.Utils as HU
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Bits
import Colors
import Hello.Utils
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
-- <at-keyword-token>, <string-token>, <bad-string-token>,
-- <delim-token>, <whitespace-token>, <CDO-token>, <CDC-token>,
-- <colon-token>, <semicolon-token>, <comma-token>, <[-token>, <]-token>,
-- <(-token>, <)-token>, <{-token>, and <}-token>.

data CssToken =
    CssTokNum CssNum            -- <number-token>
  | CssTokPerc CssNum           -- <percentage-token>
  | CssTokDim CssNum T.Text     -- <dimension-token>

  -- Ident-like-tokens (https://www.w3.org/TR/css-syntax-3/#consume-ident-like-token):
  | CssTokIdent T.Text          -- <ident-token>; CSS3 spec says that text can be empty: "have a value composed of zero or more code points".
  | CssTokFunc T.Text           -- <function-token>
  | CssTokUrl T.Text            -- <url-token>
  | CssTokBadUrl                -- <bad-url-token>

  | CssTokHash T.Text           -- <hash-token>; T.Text value is not prefixed by '#'.
  | CssTokStr T.Text
  | CssTokCh Char
  | CssTokWS          -- Whitespace
  | CssTokEnd         -- End of input. No new tokens will appear in input.
  | CssTokNone        -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show, Eq)




-- TODO: add baseUrl field.
data CssParser = CssParser {
    remainder      :: T.Text
  , spaceSeparated :: Bool
  , inBlock        :: Bool
  , bufOffset      :: Int
  , cssOrigin      :: CssOrigin -- TODO: rethink wheter origin should be a member of parser or not.
  } deriving (Show)




defaultParser = CssParser {
    remainder = ""
  , inBlock   = False
  , spaceSeparated = False
  , bufOffset = 0
  , cssOrigin = CssOriginUserAgent
  }




data CssLength = CssLength Int Int -- word (with LSB bits indicating type) + type
  deriving (Show, Eq)


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




data CssValue =
    CssValueTypeInt Int             -- This type is only used internally, for x-* properties.
  | CssValueTypeEnum Int            -- Value is i, if represented by enum_symbols[i].
  | CssValueTypeMultiEnum Int       -- For all enum_symbols[i], 1 << i are combined.
  | CssValueTypeLengthPercent CssLength   -- <length> or <percentage>. Represented by CssLength.
  | CssValueTypeLength CssLength          -- <length>, represented as CssLength.
                                    -- Note: In some cases, CSS_TYPE_LENGTH
                                    -- is used instead of
                                    -- CSS_TYPE_LENGTH_PERCENTAGE, only
                                    -- because Dw cannot handle percentages
                                    -- in this particular case (e.g.
                                    -- 'margin-*-width').
  | CssValueTypeSignedLength CssLength    -- As CSS_TYPE_LENGTH but may be negative.
  | CssValueTypeLengthPercentNumber CssLength -- <length> or <percentage>, or <number>
  | CssValueTypeAuto CssLength            -- Represented as CssLength of type cssLengthTypeAuto
  | CssValueTypeColor Int           -- Represented as integer.
  | CssValueTypeFontWeight Int      -- This very special and only used by 'font-weight'
  | CssValueTypeString T.Text       -- <string>
  | CssValueTypeStringList T.Text   -- List of symbols, which are directly
                                    -- copied (as opposed to
                                    -- CSS_PROPERTY_DATA_TYPE_ENUM and
                                    -- CSS_PROPERTY_DATA_TYPE_MULTI_ENUM).
                                    -- Used for 'font-family'. TODO: this
                                    -- should really be a Haskell list. No
                                    -- need to deconstruct the string to a
                                    -- list later.
  | CssValueTypeURI T.Text          -- <uri>
  | CssValueTypeBgPosition          -- TODO: add values to this constructor
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
   , ("background-image",       [ declValueAsURI ],                                                   [])
   , ("background-position",    [ tokensAsValueBgPosition ],                                          [])
   , ("background-repeat",      [ tokensAsValueEnum ],                                                css_background_repeat_enum_vals)
   , ("border-bottom-color",    [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-bottom-style",    [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-bottom-width",    [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals)
   , ("border-collapse",        [ tokensAsValueEnum ],                                                css_border_collapse_enum_vals)
   , ("border-left-color",      [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-left-style",      [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-left-width",      [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals)
   , ("border-right-color",     [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-right-style",     [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-rigth-width",     [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals)
   , ("border-spacing",         [ declValueAsLength ],                                                [])
   , ("border-top-color",       [ tokensAsValueEnum, tokensAsValueColor ],                            css_border_color_enum_vals)
   , ("border-top-style",       [ tokensAsValueEnum ],                                                css_border_style_enum_vals)
   , ("border-top-width",       [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals)
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
   , ("font-size",              [ tokensAsValueEnum, declValueAsLengthPercent ],                      css_font_size_enum_vals)
   , ("font-size-adjust",       [],                                                                   [])
   , ("font-stretch",           [],                                                                   [])
   , ("font-style",             [ tokensAsValueEnum ],                                                css_font_style_enum_vals)
   , ("font-variant",           [ tokensAsValueEnum ],                                                css_font_variant_enum_vals)
   , ("font-weight",            [ tokensAsValueEnum, declValueAsFontWeightInteger ],                  css_font_weight_enum_vals)
   , ("height",                 [ declValueAsLengthPercent, tokensAsValueAuto ],                      [])
   , ("left",                   [],                                                                   [])
   , ("letter-spacing",         [ tokensAsValueEnum, declValueAsSignedLength ],                       css_letter_spacing_enum_vals)
   , ("line-height",            [ tokensAsValueEnum, declValueAsLengthPercentNumber ],                 css_line_height_enum_vals)
   , ("list-style-image",       [],                                                                   [])
   , ("list-style-position",    [ tokensAsValueEnum ],                                                css_list_style_position_enum_vals)
   , ("list-style-type",        [ tokensAsValueEnum ],                                                css_list_style_type_enum_vals)
   , ("margin-bottom",          [ declValueAsSignedLength, tokensAsValueAuto ],                       [])
   , ("margin-left",            [ declValueAsSignedLength, tokensAsValueAuto ],                       [])
   , ("margin-right",           [ declValueAsSignedLength, tokensAsValueAuto ],                       [])
   , ("margin-top",             [ declValueAsSignedLength, tokensAsValueAuto ],                       [])
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
   , ("padding-bottom",         [ declValueAsLength ],                                                [])
   , ("padding-left",           [ declValueAsLength ],                                                [])
   , ("padding-right",          [ declValueAsLength ],                                                [])
   , ("padding-top",            [ declValueAsLength ],                                                [])
   , ("position",               [],                                                                   [])
   , ("quotes",                 [],                                                                   [])
   , ("right",                  [],                                                                   [])
   , ("text-align",             [ tokensAsValueEnum ],                                                css_text_align_enum_vals)
   , ("text-decoration",        [ tokensAsValueMultiEnum ],                                           css_text_decoration_enum_vals)
   , ("text-indent",            [ declValueAsLengthPercent ],                                         [])
   , ("text-shadow",            [],                                                                   [])
   , ("text-transform",         [ tokensAsValueEnum ],                                                css_text_transform_enum_vals)
   , ("top",                    [],                                                                   [])
   , ("unicode-bidi",           [],                                                                   [])
   , ("vertical-align",         [ tokensAsValueEnum ],                                                css_vertical_align_vals)
   , ("visibility",             [],                                                                   [])
   , ("white-space",            [ tokensAsValueEnum ],                                                css_white_space_vals)
   , ("width",                  [ declValueAsLengthPercent, tokensAsValueAuto ],                      [])
   , ("word-spacing",           [ tokensAsValueEnum, declValueAsSignedLength ],                       css_word_spacing_enum_vals)
   , ("z-index",                [],                                                                   [])

   -- These are extensions, for internal used, and never parsed.
   -- TODO: verify whether we need them.
   -- TODO: verify if we still need "last" property.
   , ("x-link",                 [ declValueAsInt ],                                                   [])
   , ("x-colspan",              [ declValueAsInt ],                                                   [])
   , ("x-rowspan",              [ declValueAsInt ],                                                   [])
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
                    takeIdentLikeToken      >>?
                    takeString              >>?
                    takeHashToken           >>?
                    takeCharToken




nextToken2' :: CssParser -> (CssParser, Maybe CssToken)
nextToken2' parser = takeLeadingWhite2 parser >>?
                     takeNumPercDimToken      >>?
                     takeIdentLikeToken       >>?
                     takeString               >>?
                     takeHashToken            >>?
                     takeCharToken



{-
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
-}



-- Take <ident-token> from a string.
--
-- This implementation is not very pretty. It closely resembles algorithm
-- rescribed in CSS3 spec.
takeIdentToken :: CssParser -> (CssParser, Maybe CssToken)
takeIdentToken parser = if isValidStartOfIdentifier . remainder $ parser
                        then (parserMoveBy parser ident, Just $ CssTokIdent ident)
                        else (parser, Nothing)
  where
    (ident, n) = consumeName (remainder parser) "" 0




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




takeIdentLikeToken :: CssParser -> (CssParser, Maybe CssToken)
takeIdentLikeToken p1 = case takeIdentToken p1 of
                          (_, Nothing)                      -> (p1, Nothing)
                          (p2, Just t2@(CssTokIdent ident)) -> case takeCharToken p2 of
                                                                 (_, Nothing)              -> (p2, Just t2)
                                                                 (p3, Just (CssTokCh '(')) -> if ident == "url"
                                                                                              then consumeUrlToken p3
                                                                                              else (p3, Just $ CssTokFunc ident)
                                                                 (p3, _)                   -> (p2, Just t2)



consumeUrlToken p1 = if T.length text > 0 && T.last text == ')'
                     then (p2, Just $ CssTokUrl $ T.take (n - 1) text) -- Don't include closing paren.
                     else (p2, Just $ CssTokBadUrl)
  where
    p2 = p1 {remainder = T.drop n $ remainder p1}
    text = T.pack . reverse $ f (remainder p1) []
    n = T.length text

    f :: T.Text -> [Char] -> String
    f buffer acc = case T.uncons buffer of
                 Just (c, rem) | c == ')'  -> (c:acc) -- Include the paren here to recognize valid URL.
                               | otherwise -> f rem (c:acc)
                               -- TODO: these conditions for taking chars should be improved.
                 Nothing -> acc



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
                               (value, n) = consumeName rem "" 0
                           Just (c, rem)   -> (parser, Nothing)
                           Nothing         -> (parser, Nothing)




-- https://www.w3.org/TR/css-syntax-3/#consume-a-name
--
-- Returns pair (name, count), where count is a number of consumed code
-- points.
consumeName :: T.Text -> T.Text -> Int -> (T.Text, Int)
consumeName buffer acc n = case T.uncons buffer of
                             Just (c, rem) | isNameCodePoint c -> consumeName rem (T.snoc acc c) (n + 1)
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




-- Return integer representing a color. The color is built from body of "rgb"
-- function.
rgbFunctionToColor :: CssParser -> ((CssParser, CssToken), Maybe Int)
rgbFunctionToColor p1 = let
  consumeRgbFunctionTokens = consumeFunctionTokens 5 -- 5 == count of tokens in "10%,20%,30%)", excluding closing paren.
  ((p2, t2), tokens) = consumeRgbFunctionTokens p1
  in
    case interpretRgbFunctionTokens tokens of
      Nothing                            -> ((p2, t2), Nothing)
      Just (red, green, blue, isPercent) -> ((p2, t2), Just color)
        where
          color = (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
          r = toColorComponent isPercent (fromIntegral red)
          g = toColorComponent isPercent (fromIntegral green)
          b = toColorComponent isPercent (fromIntegral blue)

          -- Convert given float (which may or may not be a percentage) into
          -- an integer in range 0x00-0xFF.
          toColorComponent :: Bool -> Float -> Int
          toColorComponent True  = clipFF . round . (\x -> ((x * 255.0) / 100.0))
          toColorComponent False = clipFF . round

          -- Ensure that given integer is in range 0x00-0xFF. Clip values that
          -- are outside of this range.
          clipFF :: Int -> Int
          clipFF x | x > 0xFF  = 0xFF
                   | x < 0     = 0
                   | otherwise = x




-- Interpret list of tokens in body of rgb functions. Extract r/g/b values
-- from the body. Let caller know if the values are in percents (0-100 range)
-- or not (0-255 range).
--
-- The last token in the list should be a paren that closes the function's
-- body - this is paren is used to recognize valid end of valid body of a
-- function.
--
-- "100%,90%,0%)" -> Just (r, g, b, True)
-- "255,14,91)"   -> Just (r, g, b, False)
interpretRgbFunctionTokens :: [CssToken] -> Maybe (Int, Int, Int, Bool)
interpretRgbFunctionTokens tokens =
  case tokens of
    -- "either three integer values or three percentage values" in https://www.w3.org/TR/css-color-3/
    (CssTokPerc (CssNumI r):CssTokCh ',':CssTokPerc (CssNumI g):CssTokCh ',':CssTokPerc (CssNumI b):CssTokCh ')':[]) -> Just (r, g, b, True)
    (CssTokNum (CssNumI r):CssTokCh ',':CssTokNum (CssNumI g):CssTokCh ',':CssTokNum (CssNumI b):CssTokCh ')':[])    -> Just (r, g, b, False)
    otherwise                                                                                                        -> Nothing




-- Take all tokens (after initial "function-name(" tokens) that belong to
-- function's body. Closing paren is added to output list (if it was present
-- in input stream) - with the closing paren you can recognize if the body is
-- complete.
--
-- https://www.w3.org/TR/css-syntax-3/#consume-function
--
-- If `limit` is non-zero, take up to `limit` tokens (excluding closing
-- paren). This is a safety feature to avoid problems with malformed input.
consumeFunctionTokens :: Int -> CssParser -> ((CssParser, CssToken), [CssToken])
consumeFunctionTokens limit p1 = ((p2, t2), reverse list)
  where
    ((p2, t2), list) = takeNext (nextToken p1) []
    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (p2, t2@(CssTokCh ')')) list = (nextToken p2, t2:list) -- Add closing paren to result, it will be used to check if function body is valid.
    takeNext (p2, CssTokEnd) list         = ((p2, CssTokEnd), list) -- https://www.w3.org/TR/css-syntax-3/#consume-function: "This is a parse error".
    takeNext (p2, t2) list                = if (limit > 0 && length list >= limit)
                                            then ((p2, t2), list)
                                            else takeNext (nextToken p2) (t2:list)




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
tokensAsValueColor (p1, (CssTokHash str)) _    = case colorsHexStringToColor str of
                                                   Just i  -> (nextToken p1, Just (CssValueTypeColor i))
                                                   Nothing -> (nextToken p1, Nothing)
tokensAsValueColor (p1, (CssTokFunc "rgb")) _  = case rgbFunctionToColor p1 of
                                                   ((p2, t2), Just i)  -> ((p2, t2), Just (CssValueTypeColor i))
                                                   ((p2, t2), Nothing) -> ((p2, t2), Nothing)
tokensAsValueColor (p1, (CssTokIdent ident)) _ = case colorsStringToColor ident of
                                                   Just i  -> (nextToken p1, Just (CssValueTypeColor i))
                                                   Nothing -> (nextToken p1, Nothing)
tokensAsValueColor (p1, t1) _                  = ((p1, t1), Nothing)





declValueAsString :: Int -> (CssParser, CssToken) -> CssPropertyInfo -> ((CssParser, CssToken), Maybe T.Text)
declValueAsString id (parser, token) propInfo = case ((retParser, retToken), value) of
                                                  ((p, t), Just (CssValueTypeString s))  -> ((p, t), Just s)
                                                  ((p, t), Nothing) -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | id == 10  = tokensAsValueString (parser, token) []
                                   | id == 12  = declValueAsURI (parser, token) []
                                   | otherwise = ((parser, token), Nothing)




-- Interpret current token as enum value (value of type CssValueTypeEnum).
--
-- In case of enum value there is no need to consume more than current token
-- to build the Enum, but for consistency with other similar functions the
-- function is still called "tokensAs...".
tokensAsValueEnum :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueEnum (parser, token@(CssTokIdent sym)) enums =
  case L.elemIndex (T.toLower sym) enums of -- TODO: should we use toLower when putting string in token or can we use it here?
    Just idx -> (nextToken parser, Just (CssValueTypeEnum idx))
    Nothing  -> ((parser, token), Nothing)
  where
tokensAsValueEnum (parser, token) _                     = ((parser, token), Nothing)
                                                          -- TODO: is this the right place to reject everything else other than symbol?
                                                          -- Shouldn't we do it somewhere else?




-- Interpret current CssTokIdent token (and possibly more following CssTokIdent
-- tokens) as multi-enum value (value of type CssValueTypeEnum). Returned
-- integer is a bit vector with bits set for enums which were matched with
-- tokens.
--
-- In case of multi-enum value it's possible that consecutive CssTokIdent
-- tokens after current CssTokIdent token will be taken and perhaps matched
-- agains given enumeration of recognized values.
--
-- If input stream contains CssTokIdent tokens with values not present in the
-- enumeration (perhaps they come from newer version of standard or perhaps
-- contain typos), then the function returns Nothing. Rationale: Firefox 78
-- and Chromium 90 don't apply this style:
-- "text-decoration: overline underline frog line-through;"
--
-- TODO: the function should be even stricter: the function should return
-- Nothing if any token in 'value' part of declaration *is not a CssTokIdent
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
tokensAsValueMultiEnum pair@(_, CssTokIdent sym) enums = case matchSymbolTokensWithListRigid pair enums 0x00 of
                                                           ((_, _), 0x00)  -> (pair, Nothing) -- None of input tokens were matched agains list of enums.
                                                           ((p2, t2), val) -> ((p2, t2), Just (CssValueTypeMultiEnum val))
tokensAsValueMultiEnum (p, t) _                        = ((p, t), Nothing)




-- Match current CssTokIdent token and any following CssTokIdent tokens against
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
matchSymbolTokensWithListRigid (p, t@(CssTokIdent sym)) enums bits =
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
tokensAsValueAuto (p, t@(CssTokIdent sym)) _ | T.toLower sym == "auto" = ((nextToken p), Just (CssValueTypeAuto (CssLength cssLengthTypeAuto cssLengthTypeAuto)))
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
                                                  pair@(p3, CssTokIdent sym)  -> if unitStringIsValid sym
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




declValueAsSignedLength :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsSignedLength (parser, token) enums = declValueAsLength' CssValueTypeSignedLength (parser, token) enums

declValueAsLengthPercent :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsLengthPercent (parser, token) enums = declValueAsLength' CssValueTypeLengthPercent (parser, token) enums

declValueAsLengthPercentNumber :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsLengthPercentNumber (parser, token) enums = declValueAsLength' CssValueTypeLengthPercentNumber (parser, token) enums

declValueAsLength :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsLength (parser, token) enums = declValueAsLength' CssValueTypeLength (parser, token) enums

declValueAsLength' :: (CssLength -> CssValue) -> (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsLength' ctor (parser, token) enums =
  case tokens of
    [CssTokDim cssNum ident] -> ((newParser, newToken), Just (ctor (unitValue cssNum ident)))
    [CssTokPerc cssNum]      -> ((newParser, newToken), Just (ctor . percentValue $ cssNum))
    [CssTokNum cssNum]       -> case ((newParser, newToken), unitlessValue cssNum) of
                                  ((p2, t2), Just i)  -> ((p2, t2), Just (ctor i))
                                  ((p2, t2), Nothing) -> ((p2, t2), Nothing)
    _                        -> ((parser, token), Nothing)
  where
    ((newParser, newToken), tokens) = takeLengthTokens (parser, token)

    percentValue :: CssNum -> CssLength
    percentValue cssNum = cssCreateLength val t
      where
        fval = cssNumToFloat cssNum
        (val, t) = ((fval / 100.0), cssLengthTypePercentage)

    unitValue :: CssNum -> T.Text -> CssLength
    unitValue cssNum unitString = cssCreateLength val t
      where
        fval = cssNumToFloat cssNum
        (val, t) = lengthValueToValAndType fval (T.toLower unitString)

    unitlessValue :: CssNum -> Maybe CssLength
    -- Allow numbers without unit only for 0 or LengthPercentNumber. TODO: why?
    unitlessValue cssNum = if (ctor (CssLength 1 cssLengthTypeNone) == CssValueTypeLengthPercentNumber (CssLength 1 cssLengthTypeNone) || fval == 0.0) -- TODO: is this the best way to compare data ctors?
                           then Just (cssCreateLength val t)
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
tokensAsValueString (p, (CssTokStr s)) _ = (nextToken p, Just (CssValueTypeString s))
tokensAsValueString (p, t) _             = ((p, t), Nothing)




declValueAsURI :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsURI (parser, token) enums = case parseUrl (parser, token) of
                                         ((newParser, newToken), Just url) -> ((newParser, newToken), Just (CssValueTypeURI url))
                                         ((newParser, newToken), Nothing)  -> ((newParser, newToken), Nothing)




parseUrl :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
parseUrl (parser, token@(CssTokIdent "url")) = (outParser, outUrl)
  where
    outUrl = case partialUrl of
      Nothing  -> Nothing
      Just url -> Just url -- TODO: here we have to add first part of URL, defined in CssParser (the one starting with e.g. http://server.com).
    (outParser, partialUrl) = case nextToken parser of
                                (newParser, newToken@(CssTokCh '(')) -> appendToUrl (newParser, newToken) ""
                                (newParser, newToken)                -> ((parser, token), Nothing)
    appendToUrl (parser, token) acc = case nextToken parser of
                                        pair@(newParser, CssTokCh ')')    -> (pair, Just acc)
                                        pair@(newParser, CssTokCh ch)     -> appendToUrl pair (T.snoc acc ch)
                                        pair@(newParser, CssTokStr str)   -> appendToUrl pair (T.concat [acc, str])
                                        pair@(newParser, CssTokIdent str) -> appendToUrl pair (T.concat [acc, str])
                                        pair@(newParser, _)               -> (pair, Nothing) -- TODO: This is a BAD URL situation
parseUrl (parser, token)   = ((parser, token), Nothing)




tokensAsValueBgPosition :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueBgPosition (parser, token) _ = ((outParser, outToken), value)
  where
    ((outParser, outToken), tokens) = takeBgTokens (parser, token)
    value = Just (CssValueTypeBgPosition)
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
    reorderTokens tokens@[CssTokIdent "top", _]    = reverse tokens -- First token should be horiz, second should be vert.
    reorderTokens tokens@[CssTokIdent "bottom", _] = reverse tokens -- First token should be horiz, second should be vert.
    reorderTokens tokens@[CssTokIdent "initial"]   = tokens -- Handle single-element "initial" first, before other single-element lists.
    reorderTokens tokens@[CssTokIdent "inherit"]   = tokens -- Handle single-element "inherit" first, before other single-element lists.
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


    -- Change CssTokIdent tokens for top/left/center etc. into <percentage-token>s.
    -- TODO: this function doesn't handle "initial" and "inherit" - what do we do with them?
    remapToken :: CssToken -> CssToken
    remapToken tok@(CssTokIdent sym) = case M.lookup sym posMap of
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
    tokValid (CssTokIdent ident)      = elem ident horizVals || elem ident vertVals || elem ident otherVals || ident == "center" -- TODO: or $ map (elem ident) [horizVals, vertVals, otherVals, ["center"]]
    tokValid (CssTokNum cssNum)       = True
    tokValid (CssTokDim cssNum ident) = True
    tokValid (CssTokPerc cssNum)      = True
    tokValid _                        = False

    horizVals = ["left", "right"]
    vertVals  = ["top", "bottom"]
    otherVals = ["initial", "inherit"]

    tokensValid [CssTokIdent sym1, CssTokIdent sym2] = cond1 && cond2 && cond3
      where
        cond1 = not (elem sym1 otherVals && elem sym2 otherVals) -- "initial" or "inherit" isn't used twice.
        cond2 = not (elem sym1 horizVals && elem sym2 horizVals) -- Both symbols aren't from the same list of horizontal tokens.
        cond3 = not (elem sym1 vertVals  && elem sym2 vertVals)  -- Both symbols aren't from the same list of vertical tokens.
    tokensValid [tok1, tok2] = True
    tokensValid [tok1]       = True -- Single-token list is valid: token's value will be used as X, and Y will be set to 50%.
    tokensValid _            = False




-- Interpret current CssTokIdent/CssTokStr token (and possibly more following
-- CssTokIdent and CssTokStr tokens) as list value (value of type
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
-- be strings with spaces, therefore the function consumes both CssTokIdent and
-- CssTokStr tokens. TODO: test the code for list of symbols separated by
-- space or comma.
tokensAsValueStringList :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueStringList (parser, token) enums = asList (parser, token) []
  where
    asList :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
    asList (p, (CssTokIdent sym)) acc = asList (nextToken p) (sym:acc)
    asList (p, (CssTokStr str)) acc   = asList (nextToken p) (str:acc)
    asList (p, (CssTokCh  ',')) acc   = asList (nextToken p) (",":acc)
    asList (p, t@(CssTokCh ';')) acc  = final (p, t) acc
    asList (p, t@(CssTokCh '}')) acc  = final (p, t) acc
    asList (p, t@(CssTokEnd)) acc     = final (p, t) acc
    asList (p, t) acc                 = ((parser, token), Nothing) -- TODO: this implmentation does not allow for final "!important" token.

    final (p, t) acc = if 0 == length acc
                       then ((p, t), Nothing)
                       else ((p, t), Just (CssValueTypeStringList . T.concat . reverse $ acc))




declValueAsFontWeightInteger :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsFontWeightInteger (parser, token@(CssTokNum (CssNumI i))) _ = if i >= 100 && i <= 900
                                                                         then ((parser, token), Just (CssValueTypeFontWeight i))
                                                                         else ((parser, token), Nothing)
declValueAsFontWeightInteger (parser, token) _                         = ((parser, token), Nothing)



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
                                                   CssTokIdent s -> if s == "center" || s == "left" || s == "right" || s == "top" || s == "bottom"
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
                                                   CssTokIdent s -> if s == "url"
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


cssLengthType :: CssLength -> Int
cssLengthType (CssLength word t) = t




cssLengthValue :: CssLength -> Float
cssLengthValue (CssLength word t) | t == cssLengthTypePX = let
                                      z = (word `shiftR` 3)
                                    in
                                      if (0xf0000000 .&. word) == 0xf0000000
                                      then fromIntegral ((-1) * ((4294967295 - word) `shiftR` 3) - 1)
                                      else fromIntegral z
                                  | t == cssLengthTypeNone
                                    || t == cssLengthTypeMM
                                    || t == cssLengthTypeEM
                                    || t == cssLengthTypeEX
                                    || t == cssLengthTypePercentage
                                    || t == cssLengthTypeRelative =
                                      (fromIntegral (up2 word)) / (fromIntegral down2)
                                  | t == cssLengthTypeAuto = 0.0
                                  | otherwise = 0.0
  where
    up2 lenA = let
      z = lenA .&. (complement 0x00000007) :: Int
      in
        if (0xf0000000 .&. z) == 0xf0000000
        then (-1) * (4294967295 - z - 1)
        else z
    down2 = 1 `shiftL` 15 :: Int





css_LENGTH_FRAC_MAX = (1 `shiftL` (32 - 15 - 1)) - 1 :: Int
css_LENGTH_INT_MAX  = (1 `shiftL` (32 - 4)) - 1 :: Int

cssCreateLength :: Float -> Int -> CssLength
cssCreateLength f t | t == cssLengthTypePX = CssLength word1 t
                    | t == cssLengthTypeNone
                      || t == cssLengthTypeMM
                      || t == cssLengthTypeEM
                      || t == cssLengthTypeEX
                      || t == cssLengthTypePercentage
                      || t == cssLengthTypeRelative = CssLength word2 t
                    | t == cssLengthTypeAuto = CssLength t t
                    | otherwise = CssLength cssLengthTypeAuto cssLengthTypeAuto

  where
    word1 = (((asInt1 (round f)) `shiftL` 3) .|. t)
    word2 = (((round ((asInt2 f) * (fromIntegral shift15L))) .&. (complement 7)) .|. t)

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
                                          (newParser, CssTokIdent "important") -> (nextToken newParser, True)
                                          (newParser, tok)                     -> ((newParser, tok), False)
cssParseWeight (parser, tok)          = ((parser, tok), False)




data CssSimpleSelector = CssSimpleSelector {
    selectorPseudoClass :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#pseudo-class
  , selectorId          :: T.Text          -- https://www.w3.org/TR/selectors-4/#id-selector
  , selectorClass       :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#class-selector
  , selectorType        :: Int             -- https://www.w3.org/TR/selectors-4/#type-selector
                                           -- TODO: add https://www.w3.org/TR/selectors-4/#universal-selector
                                           -- TODO: add https://www.w3.org/TR/selectors-4/#attribute-selector
  , combinator          :: CssCombinator
  } deriving (Show, Eq)




data CssSelector = CssSelector {
    matchCacheOffset :: Int
  , simpleSelectors  :: [CssSimpleSelector]
  } deriving (Show, Eq)




cssSimpleSelectorElementNone :: Int = (-1)
-- TODO: this probably corresponds with
-- https://www.w3.org/TR/selectors-4/#universal-selector, and should be
-- separated from type selector.
cssSimpleSelectorElementAny  :: Int  = (-2)




data CssCombinator =
    CssCombinatorNone
  | CssCombinatorDescendant        -- ' '
  | CssCombinatorChild             -- '>'
  | CssCombinatorAdjacentSibling   -- '+'
  deriving (Show, Eq)




defaultSimpleSelector = CssSimpleSelector {
    selectorPseudoClass = []
  , selectorId          = ""
  , selectorClass       = []
  , selectorType        = cssSimpleSelectorElementAny

  -- Combinator that combines this simple selector and the previous one
  -- (previous one == simple selector to the left of current simple
  -- selector). For a simple selector that is first on the list (or the only
  -- on the list), the combinator will be None.
  , combinator          = CssCombinatorNone
  }



defaultSelector = CssSelector {
    matchCacheOffset = -1
  , simpleSelectors  = [] -- [defaultSimpleSelector]
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
                             Just simSels -> ((newParser, newToken), Just defaultSelector{simpleSelectors = reverse simSels})
                             Nothing      -> ((newParser, newToken), Nothing)

    ((newParser, newToken), selectorTokens) = takeSelectorTokens (parser, token)




parseSelectorTokens :: [CssToken] -> [CssSimpleSelector] -> Maybe [CssSimpleSelector]
parseSelectorTokens (CssTokIdent sym:tokens) (simSel:simSels)  = parseSelectorTokens tokens ((simSel{selectorType = htmlTagIndex sym}):simSels)

parseSelectorTokens (CssTokCh '#':CssTokIdent sym:tokens) (simSel:simSels) = parseSelectorTokens tokens ((updateSimpleSelector simSel CssSelectorTypeID sym):simSels)
parseSelectorTokens (CssTokCh '.':CssTokIdent sym:tokens) (simSel:simSels) = parseSelectorTokens tokens ((updateSimpleSelector simSel CssSelectorTypeClass sym):simSels)
parseSelectorTokens (CssTokCh ':':CssTokIdent sym:tokens) (simSel:simSels) = parseSelectorTokens tokens ((updateSimpleSelector simSel CssSelectorTypePseudoClass sym):simSels)

parseSelectorTokens (CssTokCh '>':tokens) simSels = parseSelectorTokens tokens (defaultSimpleSelector{combinator = CssCombinatorChild}:simSels)
parseSelectorTokens (CssTokCh '+':tokens) simSels = parseSelectorTokens tokens (defaultSimpleSelector{combinator = CssCombinatorAdjacentSibling}:simSels)
parseSelectorTokens (CssTokWS:tokens)     simSels = parseSelectorTokens tokens (defaultSimpleSelector{combinator = CssCombinatorDescendant}:simSels)

parseSelectorTokens [] simSels = Just simSels
parseSelectorTokens _  simSels = Nothing




-- Parse entire list of selectors that are separated with comma.
-- Function name is matching CSS Selectors Level 4 terminology.
--
-- https://www.w3.org/TR/selectors-4/#structure:
-- "A list of simple/compound/complex selectors is a comma-separated list of
-- simple, compound, or complex selectors. This is also called just a
-- selector list when the type is either unimportant or specified in the
-- surrounding prose; if the type is important and unspecified, it defaults
-- to meaning a list of complex selectors."
--
-- Note from dillo:
--
-- TODO: dump whole ruleset in case of parse error as required by CSS 2.1
-- however make sure we don't dump it if only dillo fails to parse valid CSS.
readSelectorList :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssSelector])
readSelectorList (parser, token) = parseSelectorWrapper (parser, token) []
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




data CssDeclaration = CssDeclaration
  { property  :: Int
  , declValue :: CssValue

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
  , declValue = CssValueTypeUnused
  , important = False
  }




-- The isSafe flag compilcates this data type. I have to declare a new "Set"
-- type that is a wrapper around list of declarations + that one boolean
-- flag.
data CssDeclarationSet = CssDeclarationSet
  { isSafe :: Bool
  , items  :: S.Seq CssDeclaration
  } deriving (Show)


defaultCssDeclarationSet = CssDeclarationSet
  { isSafe = True
  , items  = S.fromList []
  }



parseDeclarationNormal :: (CssParser, CssToken) -> Int -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationNormal (parser, token) property =
  case parseDeclValue (parser, token) enums functions  of
    ((p, t), Just v)  -> ((p, t), [defaultDeclaration{property = property, declValue = v}])
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
declValueAsInt (parser, token) enums = ((parser, token), Just (CssValueTypeInt 0))





-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
parseDeclarationMultiple :: (CssParser, CssToken) -> [Int] -> [CssDeclaration] -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationMultiple (parser, token) (prop:properties) ds =
  case parseDeclValue (parser, token) enums functions of
    ((p, t), Just v)  -> parseDeclarationMultiple (p, t) properties (ds ++ [defaultDeclaration{property = prop, declValue = v}])
    ((p, t), Nothing) -> parseDeclarationMultiple (p, t) properties ds
  where
    propInfo = (cssPropertyInfo V.! prop)
    functions = tripletSnd propInfo
    enums = tripletThrd propInfo
parseDeclarationMultiple (parser, token) [] ds                = ((parser, token), ds)




parseDeclarationDirections :: (CssParser, CssToken) -> [Int] -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationDirections (parser, token) properties@(pt:pr:pb:pl:ps) = ((outParser, outToken), ds)
  where ds = case vals of
          (top:right:bottom:left:[]) -> [ defaultDeclaration{property = pt, declValue = top}
                                        , defaultDeclaration{property = pr, declValue = right}
                                        , defaultDeclaration{property = pb, declValue = bottom}
                                        , defaultDeclaration{property = pl, declValue = left}]
          (top:rl:bottom:[])         -> [ defaultDeclaration{property = pt, declValue = top}
                                        , defaultDeclaration{property = pr, declValue = rl}
                                        , defaultDeclaration{property = pb, declValue = bottom}
                                        , defaultDeclaration{property = pl, declValue = rl}]
          (tb:rl:[])                 -> [ defaultDeclaration{property = pt, declValue = tb}
                                        , defaultDeclaration{property = pr, declValue = rl}
                                        , defaultDeclaration{property = pb, declValue = tb}
                                        , defaultDeclaration{property = pl, declValue = rl}]
          (v:[])                     -> [ defaultDeclaration{property = pt, declValue = v}
                                        , defaultDeclaration{property = pr, declValue = v}
                                        , defaultDeclaration{property = pb, declValue = v}
                                        , defaultDeclaration{property = pl, declValue = v}]
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
    ((p, t), Just v)  -> parseDeclarationBorder (p, t) properties (ds ++ [ defaultDeclaration{property = top,    declValue = v}
                                                                         , defaultDeclaration{property = right,  declValue = v}
                                                                         , defaultDeclaration{property = bottom, declValue = v}
                                                                         , defaultDeclaration{property = left,   declValue = v}])

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
      (CssTokIdent _, CssTokCh ':') -> ((retParser, retToken), [nameToken]) -- Don't return ':' token. Only 'property name' token is significant to caller.
      _                             -> ((parser, nameToken), [])




-- The function returns a list of declarations because a line in CSS with a
-- shorthand declaration will be translated in N corresponding "normal"
-- declarations. E.g. "border-color: red" shorthand will be translated into a
-- list of "normal" declarations that will look like this:
-- ["border-top-color: red"; "border-right-color: red"; "border-bottom-color: red"; "border-left-color: red"]
parseDeclarationWrapper :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationWrapper (parser, token) =
  case takePropertyTokens (parser, token) of
    ((p, t), [CssTokIdent sym]) -> case cssPropertyInfoIdxByName sym of
                                     Just property -> tryNormal (p, t) property
                                     Nothing -> case cssShorthandInfoIdxByName sym of
                                                  Just shorthandIdx -> tryShorthand (p, t) shorthandIdx
                                                  Nothing -> ((p, t), [])
    ((p, t), _)                 -> ((p, t), [])




tryNormal = parseDeclarationNormal




tryShorthand (parser, token) shorthandIdx = parseDeclarationShorthand (parser, token) properties shorthandType
  where
    properties = tripletThrd $ cssShorthandInfo V.! shorthandIdx
    shorthandType = tripletSnd $ cssShorthandInfo V.! shorthandIdx





-- For non-shorthand declaration, this function should produce one-element
-- list. But a shorthand declaration translates into two or more regular
-- declarations, hence the return type contains a list of declarations.
parseDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseDeclaration (p1, t1) = ((outParser, outToken), declarationsWithImportant)
  where
    ((p2, t2), declarations) = parseDeclarationWrapper (p1, t1)
    ((p3, t3), important) = cssParseWeight (p2, t2)
    declarationsWithImportant = if important
                                then markAsImportant <$> declarations
                                else declarations
    (outParser, outToken) = consumeRestOfDeclaration (p3, t3)

    markAsImportant inDecl = inDecl{important = True}





parseDeclarationWrapper2 :: (CssParser, CssToken) -> (CssDeclarationSet, CssDeclarationSet) -> ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet))
parseDeclarationWrapper2 (p1, t1) (inSet, inSetImp) = ((p2, t2), (outSet, outSetImp))
  where
    ((p2, t2), declarations) = parseDeclaration (p1, t1)
    (outSet, outSetImp) = appendDeclarations declarations inSet inSetImp

    appendDeclarations :: [CssDeclaration] -> CssDeclarationSet -> CssDeclarationSet -> (CssDeclarationSet, CssDeclarationSet)
    appendDeclarations [] set setImp     = (set, setImp)
    appendDeclarations (d:ds) set setImp = if important d
                                           then appendDeclarations ds set (declarationsSetUpdateOrAdd setImp d)
                                           else appendDeclarations ds (declarationsSetUpdateOrAdd set d) setImp




-- Find end of current declaration (probably needed only if something goes
-- wrong during parsign of current declaration).
consumeRestOfDeclaration pair@(parser, CssTokEnd)    = pair
consumeRestOfDeclaration pair@(parser, CssTokCh '}') = pair -- '}' is not a part of declaration, so don't go past it. Return '}' as current token.
consumeRestOfDeclaration (parser, CssTokCh ';')      = nextToken parser
consumeRestOfDeclaration (parser, _)                 = consumeRestOfDeclaration . nextToken $ parser





{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




declarationsSetUpdateOrAdd :: CssDeclarationSet -> CssDeclaration -> CssDeclarationSet
declarationsSetUpdateOrAdd declSet decl =
  case S.findIndexL pred seq of
    Just idx -> CssDeclarationSet {items = S.update idx decl seq, isSafe = newSafe declSet decl}
    Nothing  -> CssDeclarationSet {items = seq S.|> decl,         isSafe = newSafe declSet decl}
  where
    pred :: CssDeclaration -> Bool
    pred x = property x == property decl

    seq = items declSet

    newSafe :: CssDeclarationSet -> CssDeclaration -> Bool
    newSafe declSet decl = (isSafe declSet)
                           && (not $ elem (property decl) [cssDeclPropertyDisplay, cssDeclPropertyBackgroundImage])




{-
Merge two disjoint sequences:

:set +m
import qualified Data.Sequence as S

let target = CssDeclarationSet {isSafe = True, items = S.fromList [
CssDeclaration {property = 1, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 11, textVal = "111"}, important = True},
CssDeclaration {property = 3, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 33, textVal = "222"}, important = False},
CssDeclaration {property = 4, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 44, textVal = "444"}, important = True}]}

let source = CssDeclarationSet {isSafe = True, items = S.fromList [
CssDeclaration {property = 7, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 77, textVal = "777"}, important = False},
CssDeclaration {property = 8, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 88, textVal = "888"}, important = True},
CssDeclaration {property = 9, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 99, textVal = "999"}, important = False}]}

let merged = declarationsSetAppend target source



Merge two sequences where the second one contains a property existing in first one (with the same value of 'property' field).

:set +m
import qualified Data.Sequence as S

let target = CssDeclarationSet {isSafe = True, items = S.fromList [
CssDeclaration {property = 1, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 11, textVal = "111"}, important = True},
CssDeclaration {property = 3, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 33, textVal = "333"}, important = False},
CssDeclaration {property = 4, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 44, textVal = "444"}, important = True}]}

let source = CssDeclarationSet {isSafe = True, items = S.fromList [
CssDeclaration {property = 7, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 77, textVal = "777"}, important = False},
CssDeclaration {property = 3, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 3333, textVal = "3333"}, important = True},
CssDeclaration {property = 9, declValue = CssValue {typeTag = CssValueTypeColor, intVal = 99, textVal = "999"}, important = False}]}

let merged = declarationsSetAppend target source
-}
{-
Concatenate values from source to target, return result of concatenation.

I can't use a concatenation operator because the concatenation is not that
simple: it has to use declarationsSetUpdateOrAdd function.
-}
declarationsSetAppend :: CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSet
declarationsSetAppend target source = if S.null . items $ source
                                      then target
                                      else declarationsSetAppend
                                           (declarationsSetUpdateOrAdd target (S.index (items source) 0))
                                           source{items = S.drop 1 (items source)}




{-
Parse CSS style information contained in "cssStyleAttribute". The buffer
contains value of "style" attribute of a html element.

import qualified Data.Sequence as S
let declSet    = CssDeclarationSet {isSafe = True, items = S.fromList []}
let declSetImp = CssDeclarationSet {isSafe = True, items = S.fromList []}
let cssStyleAttribute = "color: red !important; font-weight: bold"
parseElementStyleAttribute "" cssStyleAttribute (declSet, declSetImp)
-}
parseElementStyleAttribute :: T.Text -> T.Text -> (CssDeclarationSet, CssDeclarationSet) -> (CssDeclarationSet, CssDeclarationSet)
parseElementStyleAttribute baseUrl cssStyleAttribute (declSet, declSetImp) = (outDeclSet, outDeclSetImp)
  where
    ((p2, t2), (outDeclSet, outDeclSetImp)) = parseAllDeclarations ((p1, t1), (declSet, declSetImp))
    (p1, t1) = nextToken parser -- Kick-off the parsing

    {-
      TODO: in original C++ code the parser was initialized like this:
      CssParser parser(NULL,              -- c_css_context_t object
                       CSS_ORIGIN_AUTHOR, -- CssOrigin enum
                       baseUrl, cssStyleAttribute, buflen);
      Be sure to recreate this in final Haskell code.
    -}
    parser = CssParser{ remainder      = cssStyleAttribute
                      , spaceSeparated = False
                      , inBlock        = True -- There is no block enclosed in {}. but parser needs to behave as if we were in the block.
                      , bufOffset      = 0
                      , cssOrigin      = CssOriginAuthor
                      }




parseAllDeclarations :: ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet)) -> ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet))
parseAllDeclarations ((p1, t1), (declSet, declSetImp)) | t1 == CssTokEnd    = ((p1, t1), (declSet, declSetImp))
                                                       | t1 == CssTokCh '}' = ((p1, t1), (declSet, declSetImp))
                                                       | otherwise = parseAllDeclarations (parseDeclarationWrapper2 (p1, t1) (declSet, declSetImp))




data CssRule = CssRule {
    selector       :: CssSelector
  , declarationSet :: CssDeclarationSet
  , specificity    :: Int
  , position       :: Int
  } deriving (Show)




-- Get top simple selector
getTopSimSel :: CssRule -> CssSimpleSelector
getTopSimSel = L.last . simpleSelectors . selector




getRequiredMatchCache :: CssRule -> Int
getRequiredMatchCache rule = (matchCacheOffset . selector $ rule) + (length . simpleSelectors . selector $ rule)




-- Where does a rule come from?
data CssOrigin =
    CssOriginUserAgent -- = 0  -- Rule comes from User Agent. It is defined in program's source code.
  | CssOriginUser      -- = 1
  | CssOriginAuthor    -- = 2
  deriving (Show)




