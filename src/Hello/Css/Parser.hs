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




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Hello.Css.Parser(
                         ignoreBlock
                       , ignoreStatement

                       , cssPropertyInfo

                       , CssTypeSelector (..) -- TODO: don't export value constructors
                       , unCssTypeSelector
                       , mkCssTypeSelector
                       , styleSheetElementCount

                       , CssCompoundSelector1
                       , mkCssCompoundSelector
                       , toCompound
                       , compound2toCompound1
                       , cselTagName
                       , cselPseudoClass
                       , cselClass
                       , cselId
                       , compoundHasUniversalType
                       , compoundHasUnexpectedType
                       , compoundHasSpecificType
                       , compoundSpecificType
                       , compoundHasClass
                       , compoundHasId

                       , parseCompoundSelectorTokens

                       , parseCombinator2
                       , parseCompoundSelector2
                       , parseComplexSelector2
                       , parsePairs
                       , makeComplexR
                       , Chain (..)

                       , CssCompoundSelector2 (..)
                       , defaultCssCompoundSelector2

                       , CssSubclassSelector (..)

                       , parseUrl

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

                       , cssLengthTypeNone
                       , cssLengthTypePX
                       , cssLengthTypeMM
                       , cssLengthTypeEM
                       , cssLengthTypeEX
                       , cssLengthTypePercentage
                       , cssLengthTypeRelative
                       , cssLengthTypeAuto

                       , takeBgTokens

                       , cssShorthandInfoIdxByName
                       , cssPropertyInfoIdxByName
                       , cssPropertyNameString

                       , CssDistance (..)

                       , parseDeclarationMultiple
                       , parseDeclarationDirections
                       , parseDeclarationBorder
                       , parseDeclarationShorthand

                       , cssShorthandTypeMultiple
                       , cssShorthandTypeDirections
                       , cssShorthandTypeBorder
                       , cssShorthandTypeFont

                       , takeLengthTokens

                       , CssComplexSelector (..)
                       , defaultComplexSelector
                       , takeComplexSelectorTokens
                       , parseComplexSelector
                       , parseComplexSelectorTokens

                       , CssComplexSelectorLink (..)
                       , defaultComplexSelectorLink

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
                       , getTopCompound
                       , getRequiredMatchCache

                       , consumeFunctionBody
                       )
  where




import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Bits
import Debug.Trace

import Hello.Utils
import Hello.Css.Tokenizer
import Colors
import HtmlTag




{-
  TODO: don't hardcode the value.

  90 is the full number of html4 elements, including those which we have
  implemented. From html5, let's add: article, header, footer, mark, nav,
  section, aside, figure, figcaption, wbr, audio, video, source, embed.

  TODO: make it a constant imported from other (Html?) module
-}
styleSheetElementCount = (90 + 14) :: Int




-- This data type is not meant (yet) to be a good reflection of CSS standard.
-- For now it is only a better replacement for CssLength type. The CssLenght
-- type used an integer to encode (as bits in a bit word) different types of
-- length, with three leftmost bits used for a type tag.
--
-- This data type is a step in a better direction.
data CssDistance =
    CssDistanceRelEm Float
  | CssDistanceRelEx Float
  | CssDistanceAbsMm Float
  | CssDistanceAbsPx Float

  | CssNumericPercentage Float

  | CssNumericNone     Float
  | CssNumericRelative Float
  | CssNumericAuto     Int
  deriving (Show, Eq)



cssLengthTypeNone       = 0 :: Int
cssLengthTypePX         = 1 :: Int
cssLengthTypeMM         = 2 :: Int -- "cm", "in", "pt" and "pc" are converted into millimeters.
cssLengthTypeEM         = 3 :: Int
cssLengthTypeEX         = 4 :: Int
cssLengthTypePercentage = 5 :: Int
cssLengthTypeRelative   = 6 :: Int -- This does not exist in CSS but is used in HTML
cssLengthTypeAuto       = 7 :: Int -- This can be used as a simple value.




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
  | CssValueTypeLengthPercent CssDistance   -- <length> or <percentage>. Represented by CssDistance.
  | CssValueTypeLength CssDistance          -- <length>, represented as CssDistance.
                                    -- Note: In some cases, CSS_TYPE_LENGTH
                                    -- is used instead of
                                    -- CSS_TYPE_LENGTH_PERCENTAGE, only
                                    -- because Dw cannot handle percentages
                                    -- in this particular case (e.g.
                                    -- 'margin-*-width').
  | CssValueTypeSignedLength CssDistance    -- As CSS_TYPE_LENGTH but may be negative.
  | CssValueTypeLengthPercentNumber CssDistance -- <length> or <percentage>, or <number>
  | CssValueTypeAuto CssDistance            -- Represented as CssDistance of type CssNumericAuto
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

     -- https://www.w3.org/TR/CSS22/text.html#lining-striking-props
     -- https://www.w3.org/TR/css-text-decor-3/
     -- TODO: add support for "none" value
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
    (CssTokPerc (CssNumI r):CssTokComma:CssTokPerc (CssNumI g):CssTokComma:CssTokPerc (CssNumI b):CssTokParenClose:[]) -> Just (r, g, b, True)
    (CssTokNum (CssNumI r):CssTokComma:CssTokNum (CssNumI g):CssTokComma:CssTokNum (CssNumI b):CssTokParenClose:[])    -> Just (r, g, b, False)
    otherwise                                                                                                          -> Nothing




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
    ((p2, t2), list) = takeNext (nextToken1 p1) []
    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (p2, t2@(CssTokParenClose)) list = (nextToken1 p2, t2:list) -- Add closing paren to result, it will be used to check if function body is valid.
    takeNext (p2, CssTokEnd) list             = ((p2, CssTokEnd), list) -- https://www.w3.org/TR/css-syntax-3/#consume-function: "This is a parse error".
    takeNext (p2, t2) list                    = if (limit > 0 && length list >= limit)
                                                then ((p2, t2), list)
                                                else takeNext (nextToken1 p2) (t2:list)




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
tokensAsValueColor (p1, (CssTokHash _ str)) _  = case colorsHexStringToColor str of
                                                   Just i  -> (nextToken1 p1, Just (CssValueTypeColor i))
                                                   Nothing -> (nextToken1 p1, Nothing)
tokensAsValueColor (p1, (CssTokFunc "rgb")) _  = case rgbFunctionToColor p1 of
                                                   ((p2, t2), Just i)  -> ((p2, t2), Just (CssValueTypeColor i))
                                                   ((p2, t2), Nothing) -> ((p2, t2), Nothing)
tokensAsValueColor (p1, (CssTokIdent ident)) _ = case colorsStringToColor ident of
                                                   Just i  -> (nextToken1 p1, Just (CssValueTypeColor i))
                                                   Nothing -> (nextToken1 p1, Nothing)
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
    Just idx -> (nextToken1 parser, Just (CssValueTypeEnum idx))
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
    Just idx -> matchSymbolTokensWithListRigid (nextToken1 p) enums (bits .|. (1  `shiftL` idx))
    Nothing  -> ((p, t), 0x0) -- Given token does not match enumeration of allowed strings.
matchSymbolTokensWithListRigid (p, t) _ bits                   = ((p, t), bits)




-- Interpret current token as "auto" value (value of type CssValueTypeAuto).
--
-- In case of "auto" value there is no need to consume more than current
-- token to build the Auto, but for consistency with other similar functions
-- the function is still called "tokensAs...".
tokensAsValueAuto :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueAuto (p, t@(CssTokIdent sym)) _ | T.toLower sym == "auto" = ((nextToken1 p), Just (CssValueTypeAuto (CssNumericAuto cssLengthTypeAuto)))
                                             | otherwise               = ((p, t), Nothing)
tokensAsValueAuto (p, t) _                 = ((p, t), Nothing)




lengthValueToDistance :: Float -> T.Text -> CssDistance
lengthValueToDistance fval unitStr | unitStr == "px" = CssDistanceAbsPx fval
                                   | unitStr == "mm" = CssDistanceAbsMm fval
                                   | unitStr == "cm" = CssDistanceAbsMm (fval * 10)
                                   | unitStr == "in" = CssDistanceAbsMm (fval * 25.4)
                                   | unitStr == "pt" = CssDistanceAbsMm (fval * (25.4/72.0))
                                   | unitStr == "pc" = CssDistanceAbsMm (fval * (25.4/6.0))
                                   | unitStr == "em" = CssDistanceRelEm fval
                                   | unitStr == "ex" = CssDistanceRelEx fval
                                   | otherwise       = CssNumericNone   fval




-- TODO: this function should handle multiple values per property, like here:
-- "th{border-width:0 0 1px;".
takeLengthTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeLengthTokens (parser, token) = case token of
                                     CssTokNum  _   -> (nextToken1 parser, [token])
                                     CssTokPerc  _  -> (nextToken1 parser, [token])
                                     CssTokDim  _ _ -> (nextToken1 parser, [token])
                                     CssTokSemicolon       -> ((parser, token), [])
                                     CssTokBraceCurlyClose -> ((parser, token), [])
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
                                                  pair@(p3, CssTokDelim ';') -> (pair, [numberToken])
                                                  pair@(p3, CssTokDelim '}') -> (pair, [numberToken])
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

declValueAsLength' :: (CssDistance -> CssValue) -> (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
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

    percentValue :: CssNum -> CssDistance
    percentValue cssNum = distance
      where
        fval = cssNumToFloat cssNum
        distance = CssNumericPercentage (fval / 100.0)

    unitValue :: CssNum -> T.Text -> CssDistance
    unitValue cssNum unitString = distance
      where
        fval = cssNumToFloat cssNum
        distance = lengthValueToDistance fval (T.toLower unitString)

    unitlessValue :: CssNum -> Maybe CssDistance
    -- Allow numbers without unit only for 0 or LengthPercentNumber. TODO: why?
    unitlessValue cssNum = if (ctor (CssNumericNone 1) == CssValueTypeLengthPercentNumber (CssNumericNone 1) || fval == 0.0) -- TODO: is this the best way to compare data ctors?
                           then Just distance
                           else Nothing
      where
        fval = cssNumToFloat cssNum
        distance = CssNumericNone fval




-- Interpret current token as "string" value (value of type CssValueTypeString).
--
-- In case of "string" value there is no need to consume more than current
-- token to build the String, but for consistency with other similar
-- functions the function is still called "tokensAs...".
tokensAsValueString :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueString (p, (CssTokStr s)) _ = (nextToken1 p, Just (CssValueTypeString s))
tokensAsValueString (p, t) _             = ((p, t), Nothing)




declValueAsURI :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsURI (parser, token) enums = case parseUrl (parser, token) of
                                         ((newParser, newToken), Just url) -> ((newParser, newToken), Just (CssValueTypeURI url))
                                         ((newParser, newToken), Nothing)  -> ((newParser, newToken), Nothing)




parseUrl :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
parseUrl (p1, CssTokUrl url)    = (nextToken1 p1, Just url)
parseUrl (p1, CssTokFunc "url") = ((p2, t2), Just $ T.pack (show body))
  where
    ((p2, t2), body) = consumeFunctionBody p1 []
parseUrl (p1, token)            = ((p1, token), Nothing)




consumeFunctionBody p1 acc = case nextToken1 p1 of
                               (p2, t2@CssTokParenClose) -> (nextToken1 p2, reverse (t2:acc))
                               (p2, t2@CssTokEnd)        -> (nextToken1 p2, reverse acc) -- TODO: this is a parse error, handle the error
                               (p2, t2)                  -> consumeFunctionBody p2 (t2:acc)




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
                                                CssTokNone -> takeBgTokens' (nextToken1 parser) tokens -- Take the token, but don't append it to result
                                                _          -> takeBgTokens' (nextToken1 parser) (tokens ++ [token])
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
    asList (p, (CssTokIdent sym)) acc = asList (nextToken1 p) (sym:acc)
    asList (p, (CssTokStr str)) acc   = asList (nextToken1 p) (str:acc)
    asList (p, (CssTokComma)) acc     = asList (nextToken1 p) (",":acc)
    asList (p, t@(CssTokSemicolon)) acc       = final (p, t) acc
    asList (p, t@(CssTokBraceCurlyClose)) acc = final (p, t) acc
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




ignoreBlock :: CssParser -> (CssParser, CssToken)
ignoreBlock parser = ignoreBlock' (parser, CssTokNone) 0
  where
    ignoreBlock' (parser, tok@CssTokEnd) depth    = (parser, tok)
    ignoreBlock' (parser, CssTokBraceCurlyOpen) depth  = ignoreBlock' (nextToken1 parser) (depth + 1)
    ignoreBlock' (parser, CssTokBraceCurlyClose) depth = if depth == 1
                                                         then nextToken1 parser
                                                         else ignoreBlock' (nextToken1 parser) (depth - 1)
    ignoreBlock' (parser, tok) depth              = ignoreBlock' (nextToken1 parser) depth
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
    ignoreStatement' (parser, CssTokSemicolon)      = nextToken1 parser
    ignoreStatement' (parser, CssTokBraceCurlyOpen) = ignoreBlock parser
    ignoreStatement' (parser, tok)              = ignoreStatement' (nextToken1 parser)
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
cssParseWeight (parser, CssTokDelim '!') = case nextToken1 parser of
                                             (newParser, CssTokIdent "important") -> (nextToken1 newParser, True)
                                             (newParser, tok)                     -> ((newParser, tok), False)
cssParseWeight (parser, tok)          = ((parser, tok), False)




data CssCompoundSelector2 = CssCompoundSelector2
  { selectorPseudoClass :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#pseudo-class
  , selectorId          :: T.Text          -- https://www.w3.org/TR/selectors-4/#id-selector
  , selectorClass       :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#class-selector
  , selectorTagName     :: CssTypeSelector
                                           -- TODO: add https://www.w3.org/TR/selectors-4/#attribute-selector
  } deriving (Show, Eq)



data CssComplexSelectorLink = CssComplexSelectorLink
  { compound   :: CssCompoundSelector2
  , combinator :: CssCombinator
  } deriving (Show, Eq)




data CssComplexSelector = CssComplexSelector {
    matchCacheOffset :: Int
  , links            :: [CssComplexSelectorLink] -- Links in a chain of [Complex selector : Combinator : Complex selector : Combinator : ...] items.
  } deriving (Show, Eq)




data CssComplexSelector2
  = CssComplexSelectorA
  | CssComplexSelectorB (CssCompoundSelector1)
  | CssComplexSelectorC (CssCompoundSelector1, CssCombinator, CssComplexSelector2)




-- https://www.w3.org/TR/selectors-4/#typedef-type-selector
data CssTypeSelector
    -- Type selectors: "regular" and universal
  = CssTypeSelector Int  --   https://www.w3.org/TR/selectors-4/#type-selector; Use htmlTagIndex "text" to get the integer value.
  | CssTypeSelectorUniv  --   https://www.w3.org/TR/selectors-4/#the-universal-selector
  | CssTypeSelectorUnknown
  deriving (Show, Eq)




unCssTypeSelector :: CssTypeSelector -> Int
unCssTypeSelector (CssTypeSelector t)    = t
unCssTypeSelector CssTypeSelectorUniv    = (-2)
unCssTypeSelector CssTypeSelectorUnknown = (-1)



mkCssTypeSelector :: Int -> CssTypeSelector
mkCssTypeSelector t | t >= 0 && t < styleSheetElementCount = CssTypeSelector t
                    | t == (-2)                            = CssTypeSelectorUniv
                    | otherwise                            = CssTypeSelectorUnknown



-- https://www.w3.org/TR/selectors-4/#typedef-subclass-selector
data CssSubclassSelector
 = CssIdSelector T.Text
 | CssClassSelector T.Text
 --  | CssAttrSelector -- Unsupported for now
 | CssPseudoClassSelector T.Text
 deriving (Show, Eq)



-- https://www.w3.org/TR/selectors-4/#typedef-compound-selector
newtype CssCompoundSelector1 = CssCompoundSelector1 (CssTypeSelector, [CssSubclassSelector])
  deriving (Show, Eq)




mkCssCompoundSelector :: CssTypeSelector -> [T.Text] -> [T.Text] -> T.Text -> CssCompoundSelector1
mkCssCompoundSelector ts classIdents pseudoClassIdents idIdent = CssCompoundSelector1 (ts, classes ++ pseudoClasses ++ ids)
  where
    ids           = if T.null idIdent then [] else [CssIdSelector idIdent]
    classes       = map (\x -> CssClassSelector x) classIdents
    pseudoClasses = map (\x -> CssPseudoClassSelector x) pseudoClassIdents




toCompound :: CssComplexSelectorLink -> CssCompoundSelector1
toCompound link = CssCompoundSelector1 (t, s)
  where
    t = selectorTagName . compound $ link
    s = (f1 . selectorId . compound $ link) ++ (f2 . selectorClass . compound $ link) ++ (f3 . selectorPseudoClass . compound $ link)
    f1 x  = if T.null x then [] else [CssIdSelector x]
    f2 xs = map (\x -> CssClassSelector x) xs
    f3 xs = map (\x -> CssPseudoClassSelector x) xs




compound2toCompound1 :: CssCompoundSelector2 -> CssCompoundSelector1
compound2toCompound1 cpd2 = CssCompoundSelector1 (t, s)
  where
    t = selectorTagName cpd2
    s = (f1 . selectorId $ cpd2) ++ (f2 . selectorClass $ cpd2) ++ (f3 . selectorPseudoClass $ cpd2)
    f1 x  = if T.null x then [] else [CssIdSelector x]
    f2 xs = map (\x -> CssClassSelector x) xs
    f3 xs = map (\x -> CssPseudoClassSelector x) xs





cselTagName :: CssCompoundSelector1 -> CssTypeSelector
cselTagName (CssCompoundSelector1 (t, _)) = t



cselPseudoClass :: CssCompoundSelector1 -> [CssSubclassSelector]
cselPseudoClass (CssCompoundSelector1 (_, xs)) = filter (\x -> case x of
                                                                 (CssPseudoClassSelector t) -> True
                                                                 otherwise                  -> False) xs


cselClass :: CssCompoundSelector1 -> [CssSubclassSelector]
cselClass (CssCompoundSelector1 (_, xs)) = filter (\x -> case x of
                                                           (CssClassSelector t) -> True
                                                           otherwise            -> False) xs



cselId :: CssCompoundSelector1 -> [CssSubclassSelector]
cselId (CssCompoundSelector1 (_, xs)) = filter (\x -> case x of
                                                        (CssIdSelector t) -> True
                                                        otherwise         -> False) xs




-- Is a compound selector an 'Any' HTML tag?
compoundHasUniversalType (CssCompoundSelector1 (CssTypeSelectorUniv, _)) = True
compoundHasUniversalType _                                              = False



compoundHasUnexpectedType :: CssCompoundSelector1 -> Bool
compoundHasUnexpectedType (CssCompoundSelector1 (CssTypeSelectorUnknown, _)) = True
compoundHasUnexpectedType _                                                  = False



compoundHasSpecificType :: CssCompoundSelector1 -> Bool
compoundHasSpecificType = isJust . compoundSpecificType


-- What is the element in compound selector? Either some specific HTML tag
-- (then 'Maybe t') or Any or None (then 'Nothing').
compoundSpecificType :: CssCompoundSelector1 -> Maybe Int
compoundSpecificType (CssCompoundSelector1 (CssTypeSelector t, _)) = Just t
compoundSpecificType _                                             = Nothing



compoundHasClass :: CssCompoundSelector2 -> Bool
compoundHasClass = not . null . selectorClass



compoundHasId :: CssCompoundSelector2 -> Bool
compoundHasId = not . T.null . selectorId



data CssCombinator =
    CssCombinatorNone
  | CssCombinatorDescendant        -- ' '
  | CssCombinatorChild             -- '>'
  | CssCombinatorAdjacentSibling   -- '+'
  deriving (Show, Eq)





defaultCssCompoundSelector2 = CssCompoundSelector2
  { selectorPseudoClass = []
  , selectorId          = ""
  , selectorClass       = []
  , selectorTagName     = CssTypeSelectorUniv
  }





defaultComplexSelectorLink = CssComplexSelectorLink
  { compound = defaultCssCompoundSelector2

  -- Combinator that combines this compound selector and the previous one
  -- (previous one == compound selector to the left of current compound
  -- selector). For a compound selector that is first on the list (or the only
  -- on the list), the combinator will be None.
  , combinator          = CssCombinatorNone
  }



defaultComplexSelector = CssComplexSelector {
    matchCacheOffset = -1
  , links            = [] -- [defaultComplexSelectorLink]
  }




-- Update compound selector with given subclass selector.
appendSubclassSelector :: CssCompoundSelector2 -> CssSubclassSelector -> CssCompoundSelector2
appendSubclassSelector compound subSel =
  case subSel of
    CssClassSelector ident       -> compound {selectorClass = (selectorClass compound) ++ [ident]}
    CssPseudoClassSelector ident -> if T.null ident
                                    then compound
                                    else compound {selectorPseudoClass = (selectorPseudoClass compound) ++ [ident]}
    CssIdSelector ident          -> if selectorId compound == ""
                                    then compound {selectorId = ident}
                                    else compound  -- TODO: is this valid that we ignore new value of the field without any warning?




-- Create a comples selector from a group of tokens that are terminated by
-- ',' or '{' character.
--
-- https://www.w3.org/TR/selectors-4/#structure: "A complex selector is a
-- sequence of one or more compound selectors separated by combinators."
--
-- Function always consumes the group of tokens, regardless of
-- success/failure of the parsing.
parseComplexSelector :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssComplexSelector)
parseComplexSelector (parser, token) = ((outParser, outToken), selector)
  where
    (outParser, outToken) = consumeRestOfSelector (p2, t2)
    ((p2, t2), selector) = case parseComplexSelectorTokens (removeSpaceTokens cplxSelTokens []) [defaultComplexSelectorLink] of
                             Just xs -> ((newParser, newToken), Just defaultComplexSelector{links = reverse xs})
                             Nothing -> ((newParser, newToken), Nothing)

    ((newParser, newToken), cplxSelTokens) = takeComplexSelectorTokens (parser, token)



-- let tokens = snd . takeComplexSelectorTokens $ (defaultParser{remainder="#some_id"}, CssTokNone)
-- parseComplexSelectorTokens tokens [defaultComplexSelectorLink]


parseComplexSelectorTokens :: [CssToken] -> [CssComplexSelectorLink] -> Maybe [CssComplexSelectorLink]
parseComplexSelectorTokens tokens _ = case parseComplexSelector2 tokens of
                                        Nothing      -> Nothing
                                        Just complex -> Just $ complexToLinks complex []
  where
    complexToLinks ((Chain compound1) :>: remainder) acc = complexToLinks remainder (acc ++ [defaultComplexSelectorLink { compound = compound1, combinator = CssCombinatorChild }])
    complexToLinks ((Chain compound1) :+: remainder) acc = complexToLinks remainder (acc ++ [defaultComplexSelectorLink { compound = compound1, combinator = CssCombinatorAdjacentSibling }])
    complexToLinks ((Chain compound1) :~: remainder) acc = complexToLinks remainder (acc ++ [defaultComplexSelectorLink { compound = compound1, combinator = CssCombinatorDescendant }])
    complexToLinks (Chain compound1) acc                 = acc ++ [defaultComplexSelectorLink { compound = compound1, combinator = CssCombinatorNone }]



{-
parseComplexSelectorTokens :: [CssToken] -> [CssComplexSelectorLink] -> Maybe [CssComplexSelectorLink]
parseComplexSelectorTokens (CssTokDelim '*':tokens) (x:xs) = parseComplexSelectorTokens tokens ((setSelectorTagName x CssTypeSelectorUniv):xs)
parseComplexSelectorTokens (CssTokIdent sym:tokens) (x:xs) = case htmlTagIndex2 sym of
                                                               Just idx -> parseComplexSelectorTokens tokens ((setSelectorTagName x (CssTypeSelector idx)):xs)
                                                               Nothing  -> parseComplexSelectorTokens tokens ((setSelectorTagName x (CssTypeSelectorUnknown)):xs)
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseComplexSelectorTokens (CssTokHash CssHashId ident:tokens) (x:xs)      = parseComplexSelectorTokens
                                                                             tokens ((x { compound = appendSubclassSelector (compound x) (CssIdSelector ident)}):xs)
parseComplexSelectorTokens (CssTokDelim '.':CssTokIdent sym:tokens) (x:xs) = parseComplexSelectorTokens
                                                                             tokens ((x { compound = appendSubclassSelector (compound x) (CssClassSelector sym)}):xs)
parseComplexSelectorTokens (CssTokColon:CssTokIdent sym:tokens) (x:xs)     = parseComplexSelectorTokens
                                                                             tokens ((x { compound = appendSubclassSelector (compound x) (CssPseudoClassSelector sym)}):xs)
parseComplexSelectorTokens (CssTokDelim '>':tokens) xs = parseComplexSelectorTokens tokens (defaultComplexSelectorLink{combinator = CssCombinatorChild}:xs)
parseComplexSelectorTokens (CssTokDelim '+':tokens) xs = parseComplexSelectorTokens tokens (defaultComplexSelectorLink{combinator = CssCombinatorAdjacentSibling}:xs)
parseComplexSelectorTokens (CssTokWS:tokens)        xs = parseComplexSelectorTokens tokens (defaultComplexSelectorLink{combinator = CssCombinatorDescendant}:xs)

parseComplexSelectorTokens [] xs = Just xs
parseComplexSelectorTokens _  xs = Nothing
-}


infixr 5 :>:
infixr 5 :+:
infixr 5 :~:

data Chain a
  = Chain a
  | Chain a :>: Chain a
  | Chain a :+: Chain a
  | Chain a :~: Chain a
  deriving (Show, Read, Eq, Ord)




type CssCombinator2 = (Chain CssCompoundSelector2 -> Chain CssCompoundSelector2 -> Chain CssCompoundSelector2)




parseCombinator2 :: [CssToken] -> Maybe (CssCombinator2, [CssToken])
parseCombinator2 (CssTokDelim '>':tokens) = Just ((:>:), tokens)
parseCombinator2 (CssTokDelim '+':tokens) = Just ((:+:), tokens)
parseCombinator2 (CssTokWS:tokens)        = Just ((:~:), tokens)
parseCombinator2 _                        = Nothing




parseCompoundSelector2 :: (Maybe CssCompoundSelector2, [CssToken]) -> Maybe (CssCompoundSelector2, [CssToken])
parseCompoundSelector2 (Just compound, (CssTokDelim '*':tokens)) = parseCompoundSelector2 (Just compound, tokens)
parseCompoundSelector2 (Just compound, (CssTokIdent sym:tokens)) = case htmlTagIndex2 sym of
                                                                Just idx -> parseCompoundSelector2 (Just (setSelectorTagName2 compound (CssTypeSelector idx)), tokens)
                                                                Nothing  -> parseCompoundSelector2 (Just (setSelectorTagName2 compound (CssTypeSelectorUnknown)), tokens)
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelector2 (Just compound, (CssTokHash CssHashId ident:tokens))      = parseCompoundSelector2
                                                                                   (Just (appendSubclassSelector compound (CssIdSelector ident)), tokens)
parseCompoundSelector2 (Just compound, (CssTokDelim '.':CssTokIdent sym:tokens)) = parseCompoundSelector2
                                                                                   (Just (appendSubclassSelector compound (CssClassSelector sym)), tokens)
parseCompoundSelector2 (Just compound, (CssTokColon:CssTokIdent sym:tokens))     = parseCompoundSelector2
                                                                                   (Just (appendSubclassSelector compound (CssPseudoClassSelector sym)), tokens)
parseCompoundSelector2 (Just compound, tokens)                                   = Just (compound, tokens)
parseCompoundSelector2 (Nothing, _)                                              = Nothing




parseComplexSelector2 :: [CssToken] -> Maybe (Chain CssCompoundSelector2)
parseComplexSelector2 tokens = case parseCompoundSelector2 (Just defaultCssCompoundSelector2, tokens) of
                                 Nothing                  -> Nothing
                                 Just (compound, tokens2) -> case parsePairs tokens2 [] of
                                                               Nothing         -> Nothing
                                                               Just (_, pairs) -> Just (makeComplexR (Chain compound) pairs)




-- let pairs = [((:>:), defaultCssCompoundSelector2), ((:+:), defaultCssCompoundSelector2), ((:~:), defaultCssCompoundSelector2)]
-- let compound = Chain defaultCssCompoundSelector2
-- let complex = makeComplexR compound pairs



makeComplexR :: (Chain CssCompoundSelector2) -> [(CssCombinator2, CssCompoundSelector2)] -> Chain CssCompoundSelector2
makeComplexR compound pairs = foldr f compound pairs
  where
    f :: (CssCombinator2, CssCompoundSelector2) -> (Chain CssCompoundSelector2) -> (Chain CssCompoundSelector2)
    f x acc = (fst x) (Chain (snd x)) acc




parsePairs :: [CssToken] -> [(CssCombinator2, CssCompoundSelector2)] -> Maybe ([CssToken], [(CssCombinator2, CssCompoundSelector2)])
parsePairs [] acc   = Just ([], acc) -- There is no "combinator followed by selector" data. Don't return error here.
parsePairs tokens@(x:xs) acc = case parseCombinator2 tokens of
                                 Nothing                    -> Nothing
                                 Just (combinator, tokens2) -> case parseCompoundSelector2 (Just defaultCssCompoundSelector2, tokens2) of
                                                                 Nothing -> Nothing
                                                                 Just (compound, tokens3) -> parsePairs tokens3 ((combinator, compound):acc)




parseCompoundSelectorTokens :: [CssToken] -> CssCompoundSelector2 -> Maybe ([CssToken], CssCompoundSelector2)
parseCompoundSelectorTokens (CssTokDelim '*':tokens) compound = parseCompoundSelectorTokens tokens (setSelectorTagName2 compound CssTypeSelectorUniv)
parseCompoundSelectorTokens (CssTokIdent sym:tokens) compound = case htmlTagIndex2 sym of
                                                                  Just idx -> parseCompoundSelectorTokens tokens (setSelectorTagName2 compound (CssTypeSelector idx))
                                                                  Nothing  -> parseCompoundSelectorTokens tokens (setSelectorTagName2 compound (CssTypeSelectorUnknown))
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelectorTokens (CssTokHash CssHashId ident:tokens) compound      = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssIdSelector ident))
parseCompoundSelectorTokens (CssTokDelim '.':CssTokIdent sym:tokens) compound = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssClassSelector sym))
parseCompoundSelectorTokens (CssTokColon:CssTokIdent sym:tokens) compound     = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssPseudoClassSelector sym))
parseCompoundSelectorTokens t@(CssTokDelim '>':tokens) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokDelim '+':tokens) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokWS:tokens)        compound = Just (t, compound)

parseCompoundSelectorTokens [] compound = Just ([], compound)
parseCompoundSelectorTokens _  compound = Nothing



setSelectorTagName link t = link {compound = c { selectorTagName = t }}
  where
    c = compound link


setSelectorTagName2 compound t = compound { selectorTagName = t }


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
readSelectorList :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssComplexSelector])
readSelectorList (parser, token) = parseSelectorWrapper (parser, token) []
  where
    parseSelectorWrapper (parser, token) acc =
      case parseComplexSelector (parser, token) of
        ((parser, token), Just selector) -> case token of
                                              CssTokComma -> parseSelectorWrapper (nextToken1 parser) (acc ++ [selector])
                                              otherwise   -> ((parser, token), acc ++ [selector])
        _                                -> ((parser, token), acc)





-- Find end of current selector (probably needed only if something goes wrong
-- during parsign of current selector).
consumeRestOfSelector pair@(parser, CssTokEnd)            = pair
consumeRestOfSelector pair@(parser, CssTokBraceCurlyOpen) = pair
consumeRestOfSelector pair@(parser, CssTokComma)          = pair
consumeRestOfSelector (parser, _)                         = consumeRestOfSelector . nextToken1 $ parser




-- Take all tokens until ',' or '{' or EOF is met. The tokens will be used to
-- create list of CssComplexSelectorLinks (with separating combinators). If input
-- stream starts with whitespace, discard the whitespace (don't return token
-- for it) - leading whitespace is certainly meaningless.
takeComplexSelectorTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeComplexSelectorTokens (parser, token) = takeNext (parser, token) []
  where
    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (parser, token) tokens = case token of
                                        CssTokBraceCurlyOpen -> ((parser, token), tokens)
                                        CssTokComma          -> ((parser, token), tokens)
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
removeSpaceTokens ((CssTokWS):(CssTokDelim '+'):xs) acc = removeSpaceTokens ((CssTokDelim '+'):xs) acc
removeSpaceTokens ((CssTokDelim '+'):(CssTokWS):xs) acc = removeSpaceTokens ((CssTokDelim '+'):xs) acc
removeSpaceTokens ((CssTokWS):(CssTokDelim '>'):xs) acc = removeSpaceTokens ((CssTokDelim '>'):xs) acc
removeSpaceTokens ((CssTokDelim '>'):(CssTokWS):xs) acc = removeSpaceTokens ((CssTokDelim '>'):xs) acc
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
  let (colonParser, colonToken) = nextToken1 parser
      (retParser, retToken) = nextToken1 colonParser
  in
    case (nameToken, colonToken) of
      (CssTokIdent _, CssTokColon) -> ((retParser, retToken), [nameToken]) -- Don't return ':' token. Only 'property name' token is significant to caller.
      _                            -> ((parser, nameToken), [])




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
consumeRestOfDeclaration pair@(parser, CssTokEnd)             = pair
consumeRestOfDeclaration pair@(parser, CssTokBraceCurlyClose) = pair -- '}' is not a part of declaration, so don't go past it. Return '}' as current token.
consumeRestOfDeclaration (parser, CssTokSemicolon)            = nextToken1 parser
consumeRestOfDeclaration (parser, _)                          = consumeRestOfDeclaration . nextToken1 $ parser





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
    (p1, t1) = nextToken1 parser -- Kick-off the parsing

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
parseAllDeclarations ((p1, t1), (declSet, declSetImp)) | t1 == CssTokEnd             = ((p1, t1), (declSet, declSetImp))
                                                       | t1 == CssTokBraceCurlyClose = ((p1, t1), (declSet, declSetImp))
                                                       | otherwise = parseAllDeclarations (parseDeclarationWrapper2 (p1, t1) (declSet, declSetImp))




data CssRule = CssRule {
    complexSelector :: CssComplexSelector
  , declarationSet  :: CssDeclarationSet
  , specificity     :: Int
  , position        :: Int
  } deriving (Show)




-- Get top compound selector
getTopCompound :: CssRule -> CssCompoundSelector2
getTopCompound = compound . L.last . links . complexSelector




getRequiredMatchCache :: CssRule -> Int
getRequiredMatchCache rule = (matchCacheOffset . complexSelector $ rule) + (length . links . complexSelector $ rule)

