{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

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
                       , consumeBlock
                       , ignoreStatement

                       , cssPropertyInfo

                         -- Parsing of selector
                       , parseCompoundSelectorTokens
                       , parseCombinator
                       , parseCompoundSelector
                       , parseComplexSelectorTokens
                       , takeComplexSelectorTokens
                       , parseComplexSelector
                       , parsePairs
                       , makeComplexR
                       , readSelectorList
                       , removeSpaceTokens

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

                       , takeBgTokens

                       , parseDeclarationMultiple
                       , parseDeclarationDirections
                       , parseDeclarationBorder
                       , parseDeclarationShorthand

                       , cssShorthandTypeMultiple
                       , cssShorthandTypeDirections
                       , cssShorthandTypeBorder
                       , cssShorthandTypeFont

                       , takeLengthTokens

                       , CssCombinator (..)

                       , CssDeclWrapper (..)
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

                       , consumeFunctionBody

                       , getTopCompound
                       )
  where




import Data.Data (toConstr)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Bits
import Debug.Trace

import Hello.Utils
import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Css.Value
import Colors
import HtmlTag




css_background_attachment_enum_vals = ["scroll", "fixed"]
css_background_color_enum_vals      = ["inherit"]
css_background_repeat_enum_vals     = ["repeat", "repeat-x", "repeat-y", "no-repeat"]
css_border_collapse_enum_vals       = ["separate", "collapse"]
css_border_color_enum_vals          = ["transparent", "inherit"]
css_border_style_enum_vals          = ["none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "inherit"]
css_border_width_enum_vals          = ["thin", "medium", "thick"]
css_color_enum_vals                 = ["inherit"]
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





-- Items with empty list of functions are not supported by this implementation.
cssPropertyInfo = M.fromList [
     ("background-attachment",  (makeCssDeclarationBackgroundAttachment, [ tokensAsValueEnum ],                                                css_background_attachment_enum_vals))
   , ("background-color",       (makeCssDeclarationBackgroundColor,      [ tokensAsValueEnumString, tokensAsValueColor ],                      css_background_color_enum_vals))
   , ("background-image",       (makeCssDeclarationBackgroundImage,      [ declValueAsURI ],                                                   []))
   , ("background-position",    (makeCssDeclarationBackgroundPosition,   [ tokensAsValueBgPosition ],                                          []))
   , ("background-repeat",      (makeCssDeclarationBackgroundRepeat,     [ tokensAsValueEnum ],                                                css_background_repeat_enum_vals))
   , ("border-bottom-style",    (makeCssDeclarationBorderBottomStyle,    [ tokensAsValueEnumString ],                                          css_border_style_enum_vals))
   , ("border-bottom-width",    (makeCssDeclarationBorderBottomWidth,    [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals))
   , ("border-collapse",        (makeCssDeclarationBorderCollapse,       [ tokensAsValueEnum ],                                                css_border_collapse_enum_vals))
   , ("border-left-style",      (makeCssDeclarationBorderLeftStyle,      [ tokensAsValueEnumString ],                                          css_border_style_enum_vals))
   , ("border-left-width",      (makeCssDeclarationBorderLeftWidth,      [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals))

   , ("border-top-color",       (makeCssDeclarationBorderTopColor,       [ tokensAsValueEnumString, tokensAsValueColor ],                      css_border_color_enum_vals))
   , ("border-right-color",     (makeCssDeclarationBorderRightColor,     [ tokensAsValueEnumString, tokensAsValueColor ],                      css_border_color_enum_vals))
   , ("border-bottom-color",    (makeCssDeclarationBorderBottomColor,    [ tokensAsValueEnumString, tokensAsValueColor ],                      css_border_color_enum_vals))
   , ("border-left-color",      (makeCssDeclarationBorderLeftColor,      [ tokensAsValueEnumString, tokensAsValueColor ],                      css_border_color_enum_vals))

   , ("border-right-style",     (makeCssDeclarationBorderRightStyle,     [ tokensAsValueEnumString ],                                          css_border_style_enum_vals))
   , ("border-rigth-width",     (makeCssDeclarationBorderRightWidth,     [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals))
   , ("border-spacing",         (makeCssDeclarationBorderSpacing,        [ declValueAsLength ],                                                []))
   , ("border-top-style",       (makeCssDeclarationBorderTopStyle,       [ tokensAsValueEnumString ],                                          css_border_style_enum_vals))
   , ("border-top-width",       (makeCssDeclarationBorderTopWidth,       [ tokensAsValueEnum, declValueAsLength ],                             css_border_width_enum_vals))
   , ("bottom",                 (makeCssDeclarationBottom,               [],                                                                   []))
   , ("caption-side",           (makeCssDeclarationCaptionSide,          [],                                                                   []))
   , ("clear",                  (makeCssDeclarationClear,                [],                                                                   []))
   , ("clip",                   (makeCssDeclarationClip,                 [],                                                                   []))
   , ("color",                  (makeCssDeclarationColor,                [ tokensAsValueEnumString, tokensAsValueColor ],                      css_color_enum_vals))
   , ("content",                (makeCssDeclarationContent,              [ tokensAsValueString ],                                              []))
   , ("counter-increment",      (makeCssDeclarationCounterIncrement,     [],                                                                   []))
   , ("counter-reset",          (makeCssDeclarationCounterReset,         [],                                                                   []))
   , ("cursor",                 (makeCssDeclarationCursor,               [ tokensAsValueEnum ],                                                css_cursor_enum_vals))
   , ("direction",              (makeCssDeclarationDirection,            [],                                                                   []))
   , ("display",                (makeCssDeclarationDisplay,              [ tokensAsValueEnum ],                                                css_display_enum_vals))
   , ("empty-cells",            (makeCssDeclarationEmptyCells,           [],                                                                   []))
   , ("float",                  (makeCssDeclarationFloat,                [],                                                                   []))
   , ("font-family",            (makeCssDeclarationFontFamily,           [ tokensAsValueStringList ],                                          []))
   , ("font-size",              (makeCssDeclarationFontSize,             [ tokensAsValueEnum, declValueAsLengthPercent ],                      css_font_size_enum_vals))
   , ("font-size-adjust",       (makeCssDeclarationFontSizeAdjust,       [],                                                                   []))
   , ("font-stretch",           (makeCssDeclarationFontStretch,          [],                                                                   []))
   , ("font-style",             (makeCssDeclarationFontStyle,            [ tokensAsValueEnum ],                                                css_font_style_enum_vals))
   , ("font-variant",           (makeCssDeclarationFontVariant,          [ tokensAsValueEnum ],                                                css_font_variant_enum_vals))
   , ("font-weight",            (makeCssDeclarationFontWeight,           [ tokensAsValueEnum, declValueAsFontWeightInteger ],                  css_font_weight_enum_vals))
   , ("height",                 (makeCssDeclarationHeight,               [ declValueAsLengthPercent, tokensAsValueAuto ],                      []))
   , ("left",                   (makeCssDeclarationLeft,                 [],                                                                   []))
   , ("letter-spacing",         (makeCssDeclarationLetterSpacing,        [ tokensAsValueEnum, declValueAsSignedLength ],                       css_letter_spacing_enum_vals))
   , ("line-height",            (makeCssDeclarationLineHeight,           [ tokensAsValueEnum, declValueAsLengthPercentNumber ],                css_line_height_enum_vals))
   , ("list-style-image",       (makeCssDeclarationListStyleImage,       [],                                                                   []))
   , ("list-style-position",    (makeCssDeclarationListStylePosition,    [ tokensAsValueEnum ],                                                css_list_style_position_enum_vals))
   , ("list-style-type",        (makeCssDeclarationListStyleType,        [ tokensAsValueEnum ],                                                css_list_style_type_enum_vals))
   , ("margin-bottom",          (makeCssDeclarationMarginBottom,         [ declValueAsSignedLength, tokensAsValueAuto ],                       []))
   , ("margin-left",            (makeCssDeclarationMarginLeft,           [ declValueAsSignedLength, tokensAsValueAuto ],                       []))
   , ("margin-right",           (makeCssDeclarationMarginRight,          [ declValueAsSignedLength, tokensAsValueAuto ],                       []))
   , ("margin-top",             (makeCssDeclarationMarginTop,            [ declValueAsSignedLength, tokensAsValueAuto ],                       []))
   , ("marker-offset",          (makeCssDeclarationMarkerOffset,         [],                                                                   []))
   , ("marks",                  (makeCssDeclarationMarks,                [],                                                                   []))
   , ("max-height",             (makeCssDeclarationMaxHeight,            [],                                                                   []))
   , ("max-width",              (makeCssDeclarationMaxWidth,             [],                                                                   []))
   , ("min-height",             (makeCssDeclarationMinHeight,            [],                                                                   []))
   , ("min-width",              (makeCssDeclarationMinWidth,             [],                                                                   []))
   , ("outline-color",          (makeCssDeclarationOutlineColor,         [],                                                                   []))
   , ("outline-style",          (makeCssDeclarationOutlineStyle,         [],                                                                   []))
   , ("outline-width",          (makeCssDeclarationOutlineWidth,         [],                                                                   []))
   , ("overflow",               (makeCssDeclarationOverflow,             [],                                                                   []))
   , ("padding-bottom",         (makeCssDeclarationPaddingBottom,        [ declValueAsLength ],                                                []))
   , ("padding-left",           (makeCssDeclarationPaddingLeft,          [ declValueAsLength ],                                                []))
   , ("padding-right",          (makeCssDeclarationPaddingRight,         [ declValueAsLength ],                                                []))
   , ("padding-top",            (makeCssDeclarationPaddingTop,           [ declValueAsLength ],                                                []))
   , ("position",               (makeCssDeclarationPosition,             [],                                                                   []))
   , ("quotes",                 (makeCssDeclarationQuotes,               [],                                                                   []))
   , ("right",                  (makeCssDeclarationRight,                [],                                                                   []))
   , ("text-align",             (makeCssDeclarationTextAlign,            [ tokensAsValueEnum ],                                                css_text_align_enum_vals))

     -- https://www.w3.org/TR/CSS22/text.html#lining-striking-props
     -- https://www.w3.org/TR/css-text-decor-3/
     -- TODO: add support for "none" value
   , ("text-decoration",        (makeCssDeclarationTextDecoration,       [ tokensAsValueMultiEnum ],                                           css_text_decoration_enum_vals))

   , ("text-indent",            (makeCssDeclarationTextIndent,           [ declValueAsLengthPercent ],                                         []))
   , ("text-shadow",            (makeCssDeclarationTextShadow,           [],                                                                   []))
   , ("text-transform",         (makeCssDeclarationTextTransform,        [ tokensAsValueEnum ],                                                css_text_transform_enum_vals))
   , ("top",                    (makeCssDeclarationTop,                  [],                                                                   []))
   , ("unicode-bidi",           (makeCssDeclarationUnicodeBiDi,          [],                                                                   []))
   , ("vertical-align",         (makeCssDeclarationVerticalAlign,        [ tokensAsValueEnum ],                                                css_vertical_align_vals))
   , ("visibility",             (makeCssDeclarationVisibility,           [],                                                                   []))
   , ("white-space",            (makeCssDeclarationWhitespace,           [ tokensAsValueEnum ],                                                css_white_space_vals))
   , ("width",                  (makeCssDeclarationWidth,                [ declValueAsLengthPercent, tokensAsValueAuto ],                      []))
   , ("word-spacing",           (makeCssDeclarationWordSpacing,          [ tokensAsValueEnum, declValueAsSignedLength ],                       css_word_spacing_enum_vals))
   , ("z-index",                (makeCssDeclarationZIndex,               [],                                                                   []))

   -- These are extensions for internal use, and never parsed by CSS parser.
   -- Related CSS "pseudo-properties" are set from HTML parser.
   , ("x-link",                 (makeCssDeclarationXLink,                [ declValueAsInt ],                                                   []))
   -- TODO: verify whether we need x-colspan and x-rowspan.
   , ("x-colspan",              (makeCssDeclarationXColSpan,             [ declValueAsInt ],                                                   []))
   , ("x-rowspan",              (makeCssDeclarationXRowSpan,             [ declValueAsInt ],                                                   []))
   , ("x-lang",                 (makeCssDeclarationXLang,                [],                                                                   []))
   , ("x-img",                  (makeCssDeclarationXImg,                 [],                                                                   []))
   , ("x-tooltip",              (makeCssDeclarationXTooltip,             [],                                                                   []))
   -- TODO: verify if we still need "last" property.
   , ("last",                   (makeCssDeclaration_LAST,                [],                                                                   []))

   ] :: M.Map T.Text CssPropertyInfo




type CssPropertyValueFun = (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
type CssPropertyInfo = (CssValue -> CssDeclaration, [CssPropertyValueFun], [T.Text])




cssShorthandTypeMultiple   = 0 -- [ p1 || p2 || ...], the property pi is determined  by the type; array of properties must be terminated by CSS_PROPERTY_End.
cssShorthandTypeDirections = 1 --  <t>{1,4}; array of properties must have length 4.
cssShorthandTypeBorder     = 2 -- special, used for 'border'; array of properties must have length 12.
cssShorthandTypeFont       = 3 -- special, used for 'font'




type ShorthandInfo = (Int, [T.Text])
cssShorthandInfo = M.fromList [
    ("background",     (cssShorthandTypeMultiple,      [ "background-color", "background-image", "background-repeat", "background-attachment", "background-position" ]))

  , ("border",         (cssShorthandTypeBorder,        [ "border-top-width", "border-rigth-width", "border-bottom-width", "border-left-width",
                                                         "border-top-style", "border-right-style", "border-bottom-style", "border-left-style",
                                                         "border-top-color", "border-right-color", "border-bottom-color", "border-left-color"]))

  , ("border-bottom",  (cssShorthandTypeMultiple,      [ "border-bottom-width", "border-bottom-style",  "border-bottom-color" ]))
  , ("border-color",   (cssShorthandTypeDirections,    [ "border-top-color",    "border-right-color",   "border-bottom-color", "border-left-color" ]))
  , ("border-left",    (cssShorthandTypeMultiple,      [ "border-left-width",   "border-left-style",    "border-left-color" ]))
  , ("border-right",   (cssShorthandTypeMultiple,      [ "border-rigth-width",  "border-right-style",   "border-right-color" ]))
  , ("border-style",   (cssShorthandTypeDirections,    [ "border-top-style",    "border-right-style",   "border-bottom-style", "border-left-style" ]))
  , ("border-top",     (cssShorthandTypeMultiple,      [ "border-top-width",    "border-top-style",     "border-top-color" ]))

  , ("border-width",   (cssShorthandTypeDirections,    [ "border-top-width",    "border-rigth-width",   "border-bottom-width", "border-left-width" ]))

  , ("font",           (cssShorthandTypeFont,          [ "font-size",  "font-style", "font-variant", "font-weight", "font-family" ]))

  , ("list-style",     (cssShorthandTypeMultiple,      [ "list-style-type", "list-style-position", "list-style-image" ]))
  , ("margin",         (cssShorthandTypeDirections,    [ "margin-top",      "margin-right",        "margin-bottom",      "margin-left" ]))
  , ("outline",        (cssShorthandTypeMultiple,      [ "outline-color",   "outline-style",       "outline-width"]))

  , ("padding",        (cssShorthandTypeDirections,    [ "padding-top",     "padding-right",       "padding-bottom",     "padding-left" ]))
  ] :: M.Map T.Text ShorthandInfo




-- TODO: case-insensitive search?
cssShorthandInfoByName :: T.Text -> Maybe ShorthandInfo
cssShorthandInfoByName shorthandName = M.lookup shorthandName cssShorthandInfo
{-
  where
    p :: (T.Text, Int, [CssDeclaration]) -> Bool
    p = (\t -> (tripletFst t) == shorthandName)
-}




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





declValueAsString :: Int -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
declValueAsString id (parser, token) = case ((retParser, retToken), value) of
                                         ((p, t), Just (CssValueTypeString s))  -> ((p, t), Just s)
                                         ((p, t), Nothing) -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | id == 10  = tokensAsValueString (parser, token) [] -- TODO: magic value
                                   | id == 12  = declValueAsURI (parser, token) []      -- TODO: magic value
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




-- Interpret current token as one of allowed values and save it as value of
-- type CssValueTypeString
--
-- In case of enum value there is no need to consume more than current token
-- to build the Enum, but for consistency with other similar functions the
-- function is still called "tokensAs...".
tokensAsValueEnumString :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueEnumString (parser, token@(CssTokIdent sym)) enums =
  case L.elemIndex sym' enums of -- TODO: perhaps 'elem' would be faster?
    Just idx -> (nextToken1 parser, Just (CssValueTypeString sym'))
    Nothing  -> ((parser, token), Nothing)
  where
    sym' = T.toLower sym  -- TODO: should we use toLower when putting string in token or can we use it here?
tokensAsValueEnumString (parser, token) _                       = ((parser, token), Nothing)
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
    asList (p, (CssTokComma)) acc     = asList (nextToken1 p) acc
    asList (p, t@(CssTokSemicolon)) acc       = final (p, t) acc
    asList (p, t@(CssTokBraceCurlyClose)) acc = final (p, t) acc
    asList (p, t@(CssTokEnd)) acc     = final (p, t) acc
    asList (p, t) acc                 = ((parser, token), Nothing) -- TODO: this implmentation does not allow for final "!important" token.

    final (p, t) acc = if 0 == length acc
                       then ((p, t), Nothing)
                       else ((p, t), Just (CssValueTypeStringList . reverse $ acc))




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




-- TODO: rewrite with consumeBlock?
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




-- TODO: this function can recognize only blocks enclosed by curly braces.
-- Make the function recognize all types of Css braces.
consumeBlock :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
consumeBlock (parser, token) = consumeBlock' (parser, token) [] []
  where
    -- Last argument (braces) is used to keep track of opened/closed braces
    -- to know what is the current nesting level of blocks.
    consumeBlock' (parser, tok@CssTokEnd) tokens braces                              = ((parser, tok), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyOpen) tokens braces                       = consumeBlock' (nextToken1 parser) (CssTokBraceCurlyOpen : tokens) (CssTokBraceCurlyOpen : braces)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : []) = ((nextToken1 parser), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : xs) = consumeBlock' (nextToken1 parser) (CssTokBraceCurlyClose : tokens) xs
    consumeBlock' (parser, tok) tokens braces                                        = consumeBlock' (nextToken1 parser) (tok : tokens) braces




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




-- TODO: case-insensitive search?
cssPropertyInfoIdxByName :: T.Text -> Maybe CssPropertyInfo
cssPropertyInfoIdxByName propertyName = M.lookup propertyName cssPropertyInfo
{-
  where
    p :: (T.Text, [CssPropertyValueFun], [T.Text]) -> Bool
    p = (\t -> (tripletFst t) == propertyName)
-}



cssParseWeight :: (CssParser, CssToken) -> ((CssParser, CssToken), Bool)
cssParseWeight (parser, CssTokDelim '!') = case nextToken1 parser of
                                             (newParser, CssTokIdent "important") -> (nextToken1 newParser, True)
                                             (newParser, tok)                     -> ((newParser, tok), False)
cssParseWeight (parser, tok)          = ((parser, tok), False)




-- Update compound selector with given subclass selector.
appendSubclassSelector :: CssCompoundSelector -> CssSubclassSelector -> CssCompoundSelector
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
parseComplexSelector :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssCachedComplexSelector)
parseComplexSelector (parser, token) = ((outParser, outToken), selector)
  where
    (outParser, outToken) = consumeRestOfSelector (p2, t2)
    ((p2, t2), selector) = case parseComplexSelectorTokens (removeSpaceTokens cplxSelTokens []) of
                             Just xs -> ((newParser, newToken), Just defaultComplexSelector{chain = xs})
                             Nothing -> ((newParser, newToken), Nothing)

    ((newParser, newToken), cplxSelTokens) = takeComplexSelectorTokens (parser, token)




parseCombinator :: [CssToken] -> Maybe (CssCombinator, [CssToken])
parseCombinator (CssTokDelim '>':tokens) = Just (CssCombinatorChild, tokens)
parseCombinator (CssTokDelim '+':tokens) = Just (CssCombinatorAdjacentSibling, tokens)
parseCombinator (CssTokWS:tokens)        = Just (CssCombinatorDescendant, tokens)
parseCombinator _                        = Nothing




parseCompoundSelector :: (Maybe CssCompoundSelector, [CssToken]) -> Maybe (CssCompoundSelector, [CssToken])
parseCompoundSelector (Just compound, (CssTokDelim '*':tokens)) = parseCompoundSelector (Just compound, tokens)
parseCompoundSelector (Just compound, (CssTokIdent sym:tokens)) = case htmlTagIndex2 sym of
                                                                    Just idx -> parseCompoundSelector (Just (setSelectorTagName compound (CssTypeSelector idx)), tokens)
                                                                    Nothing  -> parseCompoundSelector (Just (setSelectorTagName compound (CssTypeSelectorUnknown)), tokens)
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelector (Just compound, (CssTokHash CssHashId ident:tokens))      = parseCompoundSelector
                                                                                  (Just (appendSubclassSelector compound (CssIdSelector ident)), tokens)
parseCompoundSelector (Just compound, (CssTokDelim '.':CssTokIdent sym:tokens)) = parseCompoundSelector
                                                                                  (Just (appendSubclassSelector compound (CssClassSelector sym)), tokens)
parseCompoundSelector (Just compound, (CssTokColon:CssTokIdent sym:tokens))     = parseCompoundSelector
                                                                                  (Just (appendSubclassSelector compound (CssPseudoClassSelector sym)), tokens)
parseCompoundSelector (Just compound, tokens)                                   = Just (compound, tokens)
parseCompoundSelector (Nothing, _)                                              = Nothing




-- First take a single compound selector. Then, in properly built complex
-- selector, there should be zero or more pairs of combinator-compound. Take
-- the pairs, and combine them with the first compound into a complex
-- selector.
parseComplexSelectorTokens :: [CssToken] -> Maybe CssComplexSelector
parseComplexSelectorTokens tokens = case parseCompoundSelector (Just defaultCssCompoundSelector, tokens) of
                                      Nothing                  -> Nothing
                                      Just (compound, tokens2) -> case parsePairs tokens2 [] of
                                                                    Nothing         -> Nothing
                                                                    Just (_, pairs) -> Just (makeComplexR (Datum compound) pairs)




makeComplexR :: CssComplexSelector -> [(CssCombinator, CssCompoundSelector)] -> CssComplexSelector
makeComplexR compound pairs = foldr f compound pairs
  where
    f :: (CssCombinator, CssCompoundSelector) -> CssComplexSelector -> CssComplexSelector
    f x acc = Link (Datum (snd x)) (fst x) acc




parsePairs :: [CssToken] -> [(CssCombinator, CssCompoundSelector)] -> Maybe ([CssToken], [(CssCombinator, CssCompoundSelector)])
parsePairs [] acc   = Just ([], acc) -- There is no "combinator followed by selector" data. Don't return error here.
parsePairs tokens acc = case parseCombinator tokens of
                          Nothing                    -> Nothing
                          Just (combinator, tokens2) -> case parseCompoundSelector (Just defaultCssCompoundSelector, tokens2) of
                                                          Nothing -> Nothing
                                                          Just (compound, tokens3) -> parsePairs tokens3 ((combinator, compound):acc)




parseCompoundSelectorTokens :: [CssToken] -> CssCompoundSelector -> Maybe ([CssToken], CssCompoundSelector)
parseCompoundSelectorTokens (CssTokDelim '*':tokens) compound = parseCompoundSelectorTokens tokens (setSelectorTagName compound CssTypeSelectorUniv)
parseCompoundSelectorTokens (CssTokIdent sym:tokens) compound = case htmlTagIndex2 sym of
                                                                  Just idx -> parseCompoundSelectorTokens tokens (setSelectorTagName compound (CssTypeSelector idx))
                                                                  Nothing  -> parseCompoundSelectorTokens tokens (setSelectorTagName compound (CssTypeSelectorUnknown))
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



setSelectorTagName compound t = compound { selectorTagName = t }


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
readSelectorList :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssCachedComplexSelector])
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
-- create list of CssCompoundSelectors (with separating combinators). If input
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




data CssDeclWrapper = CssDeclWrapper
  { property  :: CssDeclaration

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
  } deriving (Show, Eq)




defaultDeclaration = CssDeclWrapper
  { property  = CssDeclaration_LAST -- TODO: make it "CssDeclarationInvalid'; TODO: somewhere there is a code that does not set property2 field.
  , important = False
  }




-- The isSafe flag compilcates this data type. I have to declare a new "Set"
-- type that is a wrapper around list of declarations + that one boolean
-- flag.
data CssDeclarationSet = CssDeclarationSet
  { isSafe :: Bool
  , items  :: S.Seq CssDeclWrapper
  } deriving (Show, Eq)


defaultCssDeclarationSet = CssDeclarationSet
  { isSafe = True
  , items  = S.fromList []
  }




parseDeclarationNormal :: (CssParser, CssToken) -> CssPropertyInfo -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationNormal (parser, token) pinfo =
  case parseDeclValue (parser, token) enums functions  of
    ((p, t), Just v)  -> ((p, t), [defaultDeclaration{property = propMaker v}])
    ((p, t), Nothing) -> ((p, t), [])
  where
    propMaker = tripletFst pinfo
    functions = tripletSnd pinfo
    enums     = tripletThrd pinfo




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
parseDeclarationMultiple :: (CssParser, CssToken) -> [CssPropertyInfo] -> [CssDeclWrapper] -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationMultiple (parser, token) (pinfo:pinfos) ds =
  case parseDeclValue (parser, token) enums functions of
    ((p, t), Just v)  -> parseDeclarationMultiple (p, t) pinfos (ds ++ [defaultDeclaration{property = propMaker v}])
    ((p, t), Nothing) -> parseDeclarationMultiple (p, t) pinfos ds
  where
    propMaker = tripletFst pinfo
    functions = tripletSnd pinfo
    enums     = tripletThrd pinfo
parseDeclarationMultiple (parser, token) [] ds                = ((parser, token), ds)




parseDeclarationDirections :: (CssParser, CssToken) -> [CssPropertyInfo] -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationDirections (parser, token) pinfos@(pt:pr:pb:pl:ps) = ((outParser, outToken), ds)
  where ds = case vals of
          (top:right:bottom:left:[]) -> [ defaultDeclaration{property = tripletFst pt $ top    }
                                        , defaultDeclaration{property = tripletFst pr $ right  }
                                        , defaultDeclaration{property = tripletFst pb $ bottom }
                                        , defaultDeclaration{property = tripletFst pl $ left   }]
          (top:rl:bottom:[])         -> [ defaultDeclaration{property = tripletFst pt $ top    }
                                        , defaultDeclaration{property = tripletFst pr $ rl     }
                                        , defaultDeclaration{property = tripletFst pb $ bottom }
                                        , defaultDeclaration{property = tripletFst pl $ rl     }]
          (tb:rl:[])                 -> [ defaultDeclaration{property = tripletFst pt $ tb     }
                                        , defaultDeclaration{property = tripletFst pr $ rl     }
                                        , defaultDeclaration{property = tripletFst pb $ tb     }
                                        , defaultDeclaration{property = tripletFst pl $ rl     }]
          (v:[])                     -> [ defaultDeclaration{property = tripletFst pt $ v      }
                                        , defaultDeclaration{property = tripletFst pr $ v      }
                                        , defaultDeclaration{property = tripletFst pb $ v      }
                                        , defaultDeclaration{property = tripletFst pl $ v      }]
          []                         -> []
        ((outParser, outToken), vals) = matchOrderedTokens (parser, token) pinfos []
parseDeclarationDirections (parser, token) _ = ((parser, token), [])




-- Value tokens must be in proper order. Example: if property is
-- "border-color", and there are four value tokens, then tokens must
-- represent colors of "top","right","bottom","left" borders.
matchOrderedTokens :: (CssParser, CssToken) -> [CssPropertyInfo] -> [CssValue] -> ((CssParser, CssToken), [CssValue])
matchOrderedTokens (parser, token) (pinfo:pinfos) values =
  case parseDeclValue (parser, token) enums functions of
    ((p, t), Just v)  -> matchOrderedTokens (p, t) pinfos (values ++ [v])
    ((p, t), Nothing) -> ((p, t), values)
  where
    functions = tripletSnd pinfo
    enums     = tripletThrd pinfo
matchOrderedTokens (parser, token) [] values               = ((parser, token), values)




-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
parseDeclarationBorder :: (CssParser, CssToken) -> [CssPropertyInfo] -> [CssDeclWrapper] -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationBorder (parser, token) (top:right:bottom:left:pinfos) ds =
  case parseDeclValue (parser, token) enums functions  of
    ((p, t), Just v)  -> parseDeclarationBorder (p, t) pinfos (ds ++ [ defaultDeclaration{property = tripletFst top $ v    }
                                                                     , defaultDeclaration{property = tripletFst right $ v  }
                                                                     , defaultDeclaration{property = tripletFst bottom $ v }
                                                                     , defaultDeclaration{property = tripletFst left $ v   }])

    ((p, t), Nothing) -> parseDeclarationBorder (p, t) pinfos ds
  where
    functions = tripletSnd top
    enums     = tripletThrd top
parseDeclarationBorder (parser, token) [] ds                             = ((parser, token), ds)




parseDeclarationShorthand :: (CssParser, CssToken) -> [CssPropertyInfo] -> Int -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationShorthand (parser, token) pinfos shorthandType | shorthandType == cssShorthandTypeMultiple   = parseDeclarationMultiple (parser, token) pinfos []
                                                               | shorthandType == cssShorthandTypeDirections = parseDeclarationDirections (parser, token) pinfos
                                                               | shorthandType == cssShorthandTypeBorder     = parseDeclarationBorder (parser, token) pinfos []
                                                               | shorthandType == cssShorthandTypeFont       = parseDeclarationMultiple (parser, token) pinfos []
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
parseDeclarationWrapper :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationWrapper (parser, token) =
  case takePropertyTokens (parser, token) of
    ((p, t), [CssTokIdent sym]) -> case cssPropertyInfoIdxByName sym of
                                     Just pinfo -> tryNormal (p, t) pinfo
                                     Nothing -> case cssShorthandInfoByName sym of
                                                  Just sinfo -> tryShorthand (p, t) sinfo
                                                  Nothing    -> ((p, t), [])
    ((p, t), _)                 -> ((p, t), [])




tryNormal :: (CssParser, CssToken) -> CssPropertyInfo -> ((CssParser, CssToken), [CssDeclWrapper])
tryNormal = parseDeclarationNormal




tryShorthand :: (CssParser, CssToken) -> ShorthandInfo -> ((CssParser, CssToken), [CssDeclWrapper])
tryShorthand (parser, token) sinfo = parseDeclarationShorthand (parser, token) pinfos shorthandType
  where
    pinfos        = map fun properties
    properties    = snd sinfo
    shorthandType = fst sinfo

    fun :: T.Text -> CssPropertyInfo
    fun property = case M.lookup property cssPropertyInfo of
                     Just pinfo -> pinfo
                     Nothing    -> (makeCssDeclaration_LAST, [], [])




-- For non-shorthand declaration, this function should produce one-element
-- list. But a shorthand declaration translates into two or more regular
-- declarations, hence the return type contains a list of declarations.
parseDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclWrapper])
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

    appendDeclarations :: [CssDeclWrapper] -> CssDeclarationSet -> CssDeclarationSet -> (CssDeclarationSet, CssDeclarationSet)
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




declarationsSetUpdateOrAdd :: CssDeclarationSet -> CssDeclWrapper -> CssDeclarationSet
declarationsSetUpdateOrAdd declSet decl =
  case S.findIndexL pred seq of
    Just idx -> CssDeclarationSet {items = S.update idx decl seq, isSafe = newSafe declSet decl}
    Nothing  -> CssDeclarationSet {items = seq S.|> decl,         isSafe = newSafe declSet decl}
  where
    -- Use 'toConstr' to compare constructors, but values without passed to constructors.
    -- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell
    pred :: CssDeclWrapper -> Bool
    pred x = (toConstr . property $ x) == (toConstr . property $ decl)

    seq = items declSet

    newSafe :: CssDeclarationSet -> CssDeclWrapper -> Bool
    newSafe declSet decl = (isSafe declSet) && case property decl of
                                                 CssDeclarationDisplay _         -> False
                                                 CssDeclarationBackgroundImage _ -> False
                                                 otherwise                       -> True




{-
Merge values from incoming into target, return result of merging

I can't use a concatenation operator because the merging is not that
simple: it has to use declarationsSetUpdateOrAdd function.
-}
declarationsSetAppend :: CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSet
declarationsSetAppend target incoming = if S.null . items $ incoming
                                        then target
                                        else declarationsSetAppend (declarationsSetUpdateOrAdd target iHead) iTail
  where
    iHead = S.index (items incoming) 0
    iTail = incoming {items = S.drop 1 (items incoming)}




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
    complexSelector :: CssCachedComplexSelector
  , declarationSet  :: CssDeclarationSet
  , specificity     :: Int
  , position        :: Int
  } deriving (Eq)


instance Show CssRule where
  show (CssRule cs ds s p) = "Rule {" ++  (show cs) ++ "\n" ++
                                          (show ds) ++ "\n" ++
                             "spec = " ++ (show s)  ++ "\n" ++
                             "pos = "  ++ (show p)  ++ "}\n"


-- Get top compound selector
getTopCompound :: CssRule -> CssCompoundSelector
getTopCompound rule = getTopCompound' . chain . complexSelector $ rule
getTopCompound' (Link (Datum c) _ remainder) = c
getTopCompound' (Datum c)                    = c

