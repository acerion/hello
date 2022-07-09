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

This file is derived from dillo-3.0.5/src/styleengine.cc.
Copyright assignments from the file:
 * Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.StyleEngine
  (
    styleEngineComputeAbsoluteLengthValue

  , styleEngineApplyStyleToFont

  , styleEngineSetFontFamily
  , styleEngineSetFontWeight
  , styleEngineSetFontSize
  , styleEngineSetFontSize'
  , styleEngineSetFontStyle
  , styleEngineSetLetterSpacing
  , styleEngineSetFontVariant

  , styleEngineComputeBorderWidth

  , styleEngineSetStyle
  , styleEngineApplyStyleToGivenNode

  , styleEngineCalculateDwLength

  , styleEngineInheritNonCssHints
  )
where




import Prelude
import Data.Bits
import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T
import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.Parser

import Hello.Dw.DwLength
import Hello.Dw.FontAttrs
import Hello.Dw.Style

import Hello.Preferences
import Hello.Utils




-- Recalculate (relative) CSS distance to absolute value
--
-- TODO: should this function return Maybe Float or Maybe Int? Original dillo
-- function returned Int.
--
-- TODO: there is a lot of Float/Int conversion here, make sure that the
-- conversions are correct from CSS standard's point of view.
--
-- TODO: it's a bit strange that a function calculating length value (in
-- theory for any html element) is accepting arguments specific to font (font
-- size and font X heigth arguments). is it because of EM and EX distance
-- types?
styleEngineComputeAbsoluteLengthValue :: CssDistance -> FontAttrs -> Int -> Float -> Float -> Maybe Float
styleEngineComputeAbsoluteLengthValue distance fontAttrs referenceValue dpiX dpiY =
  case distance of
    CssDistanceAbsPx d     -> Just d
    CssDistanceAbsMm d     -> Just (fromIntegral (roundInt (d * dpmm)))
    CssDistanceRelEm d     -> Just (fromIntegral (roundInt (d * (fromIntegral . fontSize $ fontAttrs))))
    CssDistanceRelEx d     -> Just (fromIntegral (roundInt (d * (fromIntegral . fontXHeight $ fontAttrs))))
    CssNumericNone _       -> Just 0
                              -- Length values other than 0 without unit are
                              -- only allowed in special cases (line-height)
                              -- and have to be handled separately.
                              --
                              -- TODO this line should be uncommented
                              -- assert ((int) cpp_cssLengthValue(value) == 0);
    CssNumericPercentage d -> Just (fromIntegral (roundInt (d * (fromIntegral referenceValue))))
    CssNumericRelative _   -> Nothing
    CssNumericAuto _       -> Nothing

  where
    -- Assume dpiX == dpiY
    --
    -- TODO: is this assumption always correct? Data in
    -- src/Hello/Tests/Css/StyleEngine.hs shows that it's not correct.
    dpmm = dpiX / 25.4




-- https://www.w3schools.com/cssref/pr_font_font-family.asp
-- https://developer.mozilla.org/pl/docs/Web/CSS/font-family
styleEngineSetFontFamily :: CssValue -> Preferences -> FontAttrs -> FontAttrs
styleEngineSetFontFamily value prefs fontAttrs = case value of
                                                   CssValueTypeStringList xs -> setName xs prefs fontAttrs
                                                   otherwise                 -> fontAttrs

  where
    setName (x:xs) prefs fontAttrs | x == "serif"      = fontAttrs { fontName = prefsFontSerif prefs }
                                   | x == "sans-serif" = fontAttrs { fontName = prefsFontSansSerif prefs }
                                   | x == "cursive"    = fontAttrs { fontName = prefsFontCursive prefs }
                                   | x == "fantasy"    = fontAttrs { fontName = prefsFontFantasy prefs }
                                   | x == "monospace"  = fontAttrs { fontName = prefsFontMonospace prefs }
                                   | fontExists x      = fontAttrs { fontName = x }
                                   | otherwise         = setName xs prefs fontAttrs
    setName [] _ _                                     = fontAttrs

    -- TODO: implement lookup of font name in Operating System. In dillo this
    -- has been done through Font::exists(layout, c_value->c_text_val).
    --
    -- For now this program doesn't access list of fonts available on OS, so
    -- fontExists must always return False.
    fontExists _ = False




styleEngineSetFontWeight :: CssValue -> FontAttrs -> FontAttrs
styleEngineSetFontWeight value attrs = clipWeight . setWeight $ attrs
  where
    setWeight attrs = case value of
                        CssValueTypeEnum       i -> byEnum attrs i
                        -- Per Parser.hs, a special type for weight. TODO: do we need this type?
                        CssValueTypeFontWeight i -> attrs { fontWeight = i }
                        otherwise                -> attrs

    -- TODO: the limit may be 1000, not 900.
    clipWeight a | fontWeight a < 100 = a { fontWeight = 100 }
                 | fontWeight a > 900 = a { fontWeight = 900 }
                 | otherwise          = a

    byEnum :: FontAttrs -> Int -> FontAttrs
    byEnum attrs i | i == css_FONT_WEIGHT_BOLD    = attrs { fontWeight = 700 }
                   | i == css_FONT_WEIGHT_BOLDER  = attrs { fontWeight = (fontWeight attrs) + 300 }
                   | i == css_FONT_WEIGHT_LIGHT   = attrs { fontWeight = 100 }
                   | i == css_FONT_WEIGHT_LIGHTER = attrs { fontWeight = (fontWeight attrs) - 300 }
                   | i == css_FONT_WEIGHT_NORMAL  = attrs { fontWeight = 400 }
                   | otherwise                    = attrs




data FontSize = FontSizeXXSmall
              | FontSizeXSmall
              | FontSizeSmall
              | FontSizeMedium
              | FontSizeLarge
              | FontSizeXLarge
              | FontSizeXXLarge
              | FontSizeSmaller
              | FontSizeLarger
              | FontSize CssDistance

css_FONT_SIZE_LARGE    = 0
css_FONT_SIZE_LARGER   = 1
css_FONT_SIZE_MEDIUM   = 2
css_FONT_SIZE_SMALL    = 3
css_FONT_SIZE_SMALLER  = 4
css_FONT_SIZE_XX_LARGE = 5
css_FONT_SIZE_XX_SMALL = 6
css_FONT_SIZE_X_LARGE  = 7
css_FONT_SIZE_X_SMALL  = 8




styleEngineSetFontSize' :: CssValue -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineSetFontSize' value prefs dpiX dpiY parentFontAttrs fontAttrs = styleEngineSetFontSize input prefs dpiX dpiY parentFontAttrs fontAttrs
  where
    input = case value of
              CssValueTypeEnum enum              -> integerToFontSize enum
              CssValueTypeLengthPercent distance -> Just $ FontSize distance

    integerToFontSize e | e == css_FONT_SIZE_XX_SMALL = Just FontSizeXXSmall
                        | e == css_FONT_SIZE_X_SMALL  = Just FontSizeXSmall
                        | e == css_FONT_SIZE_SMALL    = Just FontSizeSmall
                        | e == css_FONT_SIZE_MEDIUM   = Just FontSizeMedium
                        | e == css_FONT_SIZE_LARGE    = Just FontSizeLarge
                        | e == css_FONT_SIZE_X_LARGE  = Just FontSizeXLarge
                        | e == css_FONT_SIZE_XX_LARGE = Just FontSizeXXLarge
                        | e == css_FONT_SIZE_SMALLER  = Just FontSizeSmaller
                        | e == css_FONT_SIZE_LARGER   = Just FontSizeLarger
                        | otherwise                   = Nothing




-- https://developer.mozilla.org/pl/docs/Web/CSS/font-size
-- https://www.w3schools.com/cssref/pr_font_font-size.asp
styleEngineSetFontSize :: Maybe FontSize -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineSetFontSize input prefs dpiX dpiY parentFontAttrs fontAttrs = clipSize (setAbsSize input fontAttrs)
  where
    setAbsSize :: Maybe FontSize -> FontAttrs -> FontAttrs
    setAbsSize input fontAttrs = case fontSizeToAbs' input prefs dpiX dpiY fontAttrs parentFontAttrs of
                                   Just size -> fontAttrs { fontSize = size }
                                   otherwise -> fontAttrs

    clipSize :: FontAttrs -> FontAttrs
    clipSize a | fontSize a < prefsFontMinSize prefs = a { fontSize = prefsFontMinSize prefs }
               | fontSize a > prefsFontMaxSize prefs = a { fontSize = prefsFontMaxSize prefs }
               | otherwise                           = a




fontSizeToAbs :: FontSize -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> Maybe Int
fontSizeToAbs input prefs dpiX dpiY fontAttrs parentFontAttrs = case input of
                                                                  FontSizeXXSmall   -> Just $ roundInt ( 8.1  * (prefsFontFactor prefs))
                                                                  FontSizeXSmall    -> Just $ roundInt ( 9.7  * (prefsFontFactor prefs))
                                                                  FontSizeSmall     -> Just $ roundInt (11.7  * (prefsFontFactor prefs))
                                                                  FontSizeMedium    -> Just $ roundInt (14.0  * (prefsFontFactor prefs))
                                                                  FontSizeLarge     -> Just $ roundInt (16.8  * (prefsFontFactor prefs))
                                                                  FontSizeXLarge    -> Just $ roundInt (20.2  * (prefsFontFactor prefs))
                                                                  FontSizeXXLarge   -> Just $ roundInt (24.2  * (prefsFontFactor prefs))
                                                                  FontSizeSmaller   -> Just $ roundInt ( 0.83 * (fromIntegral . fontSize $ fontAttrs))
                                                                  FontSizeLarger    -> Just $ roundInt ( 1.2  * (fromIntegral . fontSize $ fontAttrs))
                                                                  FontSize distance -> case size of
                                                                                         Just s  -> Just $ roundInt s
                                                                                         Nothing -> Nothing
                                                                    where
                                                                      size           = styleEngineComputeAbsoluteLengthValue distance parentFontAttrs referenceValue dpiX dpiY
                                                                      referenceValue = fontSize parentFontAttrs




fontSizeToAbs' :: Maybe FontSize -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> Maybe Int
fontSizeToAbs' input prefs dpiX dpiY fontAttrs parentFontAttrs = case input of
                                                                   Just s -> fontSizeToAbs s prefs dpiX dpiY fontAttrs parentFontAttrs
                                                                   otherwise -> Nothing




-- https://developer.mozilla.org/pl/docs/Web/CSS/font-style
-- https://www.w3schools.com/cssref/pr_font_font-style.asp
styleEngineSetFontStyle :: CssValue -> FontAttrs -> FontAttrs
styleEngineSetFontStyle value fontAttrs = case value of
                                            CssValueTypeEnum idx -> fontAttrs { fontStyle = idx }
                                            otherwise            -> fontAttrs




css_LETTER_SPACING_NORMAL = 0

styleEngineSetLetterSpacing :: CssValue -> Float -> Float -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineSetLetterSpacing value dpiX dpiY parentFontAttrs fontAttrs = clipSpacing . setSpacing $ fontAttrs
  where
    setSpacing :: FontAttrs -> FontAttrs
    setSpacing fontAttrs = case value of
                             CssValueTypeEnum idx | idx == css_LETTER_SPACING_NORMAL -> fontAttrs { fontLetterSpacing = 0 }
                                                  | otherwise                        -> fontAttrs
                             CssValueTypeSignedLength distance -> case size of
                                                                    Just s  -> fontAttrs { fontLetterSpacing = roundInt s }
                                                                    Nothing -> fontAttrs
                               where
                                 size           = styleEngineComputeAbsoluteLengthValue distance parentFontAttrs referenceValue dpiX dpiY
                                 referenceValue = fontSize parentFontAttrs
                             otherwise                         -> fontAttrs

    --Limit letterSpacing to reasonable values to avoid overflows e.g, when
    --measuring word width.
    clipSpacing :: FontAttrs -> FontAttrs
    clipSpacing a | fontLetterSpacing a < -1000 = a { fontLetterSpacing = -1000 }
                  | fontLetterSpacing a >  1000 = a { fontLetterSpacing =  1000 }
                  | otherwise                   = a




-- https://www.w3schools.com/cssref/pr_font_font-variant.asp
styleEngineSetFontVariant :: CssValue -> FontAttrs -> FontAttrs
styleEngineSetFontVariant value fontAttrs = case value of
                                              CssValueTypeEnum idx -> fontAttrs { fontVariant = idx }
                                              otherwise            -> fontAttrs





styleEngineApplyStyleToFont :: CssDeclarationSet -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineApplyStyleToFont declSet prefs dpiX dpiY parentFontAttrs fontAttrs = apply (items declSet) prefs dpiX dpiY parentFontAttrs fontAttrs
  where
    apply :: S.Seq CssDeclWrapper -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> FontAttrs
    apply decls prefs dpiX dpiY parentFontAttrs fontAttrs =
      case S.null decls of
        True -> fontAttrs
        False  | property x == CssDeclarationFontFamily    {- 32 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontFamily value prefs fontAttrs
               | property x == CssDeclarationFontSize      {- 33 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontSize' value prefs dpiX dpiY parentFontAttrs fontAttrs
               | property x == CssDeclarationFontStyle     {- 36 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontStyle value fontAttrs
               | property x == CssDeclarationFontVariant   {- 37 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontVariant value fontAttrs
               | property x == CssDeclarationFontWeight    {- 38 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontWeight value fontAttrs
               | property x == CssDeclarationLetterSpacing {- 41 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetLetterSpacing value dpiX dpiY parentFontAttrs fontAttrs
               | otherwise                                  -> apply xs prefs dpiX dpiY parentFontAttrs $ fontAttrs
          where
            x  :: CssDeclWrapper       = S.index decls 0
            xs :: S.Seq CssDeclWrapper = S.drop 1 decls
            value :: CssValue = declValue x




css_BORDER_WIDTH_THIN   = 0
css_BORDER_WIDTH_MEDIUM = 1
css_BORDER_WIDTH_THICK  = 2

styleEngineComputeBorderWidth :: CssValue -> Float -> Float -> FontAttrs -> Maybe Int
styleEngineComputeBorderWidth value dpiX dpiY fontAttrs =
  case value of
    CssValueTypeEnum i | i == css_BORDER_WIDTH_THIN   -> Just 1
                       | i == css_BORDER_WIDTH_MEDIUM -> Just 2
                       | i == css_BORDER_WIDTH_THICK  -> Just 3
                       | otherwise                    -> Nothing
    CssValueTypeLength distance         -> fmap roundInt $ styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY
                                           -- TODO: re-think value returned by
                                           -- styleEngineComputeAbsoluteLengthValue:
                                           -- it most probably should be Int, not
                                           -- Float. Then the Float->int conversion
                                           -- won't be necessary.
    CssValueTypeLengthPercent distance -> fmap roundInt $ styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY
    otherwise                          -> trace ("[EE] unhandled css value type " ++ (show value)) Nothing -- TODO: handle all value types




{-
styleEngineSetBorderStyle :: Int -> CssValue -> StyleBorderStyle -> StyleBorderStyle
styleEngineSetBorderStyle property value borderStyle
  | property == cssDeclPropertyBorderTopStyle    = borderStyle { styleBorderStyleTop    = style }
  | property == cssDeclPropertyBorderRightStyle  = borderStyle { styleBorderStyleRight  = style }
  | property == cssDeclPropertyBorderBottomStyle = borderStyle { styleBorderStyleBottom = style }
  | property == cssDeclPropertyBorderLeftStyle   = borderStyle { styleBorderStyleLeft   = style }
  | otherwise                                    = borderStyle
  where
    style = case value of
              CssValueTypeEnum i -> i
              otherwise          -> 0




styleEngineSetBorderWidth :: Int -> CssValue -> Float -> Float -> FontAttrs -> StyleBorderWidth -> StyleBorderWidth
styleEngineSetBorderWidth property value dpiX dpiY fontAttrs borderWidth
  | property == cssDeclPropertyBorderTopWidth    = borderWidth { styleBorderWidthTop    = width }
  | property == cssDeclPropertyBorderRightWidth  = borderWidth { styleBorderWidthRight  = width }
  | property == cssDeclPropertyBorderBottomWidth = borderWidth { styleBorderWidthBottom = width }
  | property == cssDeclPropertyBorderLeftWidth   = borderWidth { styleBorderWidthLeft   = width }
  | otherwise                                    = borderWidth
  where
    width = case styleEngineComputeBorderWidth value dpiX dpiY fontAttrs of
              -- TODO: another place where Maybe returned by Compute function
              -- causes unnecessary trouble.
              Just x  -> x
              Nothing -> 0




styleEngineSetMargin :: Int -> CssValue -> Float -> Float -> FontAttrs -> StyleMargin -> StyleMargin
styleEngineSetMargin property value dpiX dpiY fontAttrs margin
  | property == cssDeclPropertyMarginBottom = margin { styleMarginBottom = clip m }
  | property == cssDeclPropertyMarginLeft   = margin { styleMarginLeft   = clip m }
  | property == cssDeclPropertyMarginRight  = margin { styleMarginRight  = clip m }
  | property == cssDeclPropertyMarginTop    = margin { styleMarginTop    = clip m }
  | otherwise                               = margin
  where
    clip x = if x > 0 then x else 0   -- TODO: fix negative margins in dw/*
    m = case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY of
          -- TODO: another place where Maybe returned by Compute function
          -- causes unnecessary trouble.
          Just x  -> roundInt x
          Nothing -> 0

    distance = case value of
                 CssValueTypeSignedLength d -> d
                 CssValueTypeAuto d         -> d -- TODO: 'auto' appears to be handled incorrectly this function




styleEngineSetPadding :: Int -> CssValue -> Float -> Float -> FontAttrs -> StylePadding -> StylePadding
styleEngineSetPadding property value dpiX dpiY fontAttrs padding
  | property == cssDeclPropertyPaddingBottom = padding { stylePaddingBottom = p }
  | property == cssDeclPropertyPaddingLeft   = padding { stylePaddingLeft   = p }
  | property == cssDeclPropertyPaddingRight  = padding { stylePaddingRight  = p }
  | property == cssDeclPropertyPaddingTop    = padding { stylePaddingTop    = p }
  | otherwise                                = padding
  where
    p = case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY of
          -- TODO: another place where Maybe returned by Compute function
          -- causes unnecessary trouble.
          Just x  -> roundInt x
          Nothing -> 0

    distance = case value of
                 CssValueTypeLength d -> d
-}




styleEngineCalculateDwLength :: CssDistance -> FontAttrs -> Float -> Float -> Maybe DwLength
styleEngineCalculateDwLength distance fontAttrs dpiX dpiY =
  case distance of
    CssNumericPercentage v -> Just $ createPercentageDwLength (realToFrac v)
    CssNumericAuto _       -> Just createAutoDwLength -- TODO: why the value of Auto is ignored?
    otherwise              -> case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY of
                                Just val -> Just $ createAbsoluteDwLength (round val) -- TODO: a type of Float -> Int function to be verified here
                                Nothing  -> Nothing




styleEngineApplyStyleToGivenNode :: CssDeclarationSet -> Preferences -> Float -> Float -> StyleAttrs -> StyleAttrs -> StyleAttrs
styleEngineApplyStyleToGivenNode declSet prefs dpiX dpiY parentStyleAttrs styleAttrs = styleAttrs'
  where
    -- Determine font first so it can be used to resolve relative lengths of other elements.
    fontAttrs'  = styleEngineApplyStyleToFont declSet prefs dpiX dpiY (styleFontAttrs parentStyleAttrs) (styleFontAttrs styleAttrs)
    styleAttrs' = setRemainingAttrs (items declSet) dpiX dpiY styleAttrs { styleFontAttrs = fontAttrs' }

    setRemainingAttrs :: S.Seq CssDeclWrapper -> Float -> Float -> StyleAttrs -> StyleAttrs
    setRemainingAttrs decls dpiX dpiY styleAttrs =
      case S.null decls of
        True  -> styleAttrs
        False -> setRemainingAttrs xs dpiX dpiY $ styleEngineSetStyle (property x) value dpiX dpiY styleAttrs
          where
            x  :: CssDeclWrapper       = S.index decls 0
            xs :: S.Seq CssDeclWrapper = S.drop 1 decls
            value :: CssValue = declValue x




styleEngineSetStyle :: CssDeclaration -> CssValue -> Float -> Float -> StyleAttrs -> StyleAttrs
styleEngineSetStyle declaration value dpiX dpiY styleAttrs
{-
TODO: re-implement these missing cases from C++. They were not re-implemented
yet because a full support for them in dillo seems to be missing or broken.

         case CSS_PROPERTY_BACKGROUND_ATTACHMENT:
            attrs->backgroundAttachment = (BackgroundAttachment) decl->c_value->c_int_val;
            break;
         case CSS_PROPERTY_BACKGROUND_IMAGE:
            // decl->value.c_text_val should be absolute, so baseUrl is not needed
            DilloUrl *imgUrl = imgUrl = a_Url_new (decl->c_value->c_text_val, NULL);
            break;
         case CSS_PROPERTY_BACKGROUND_POSITION:
            CssLength cssLength;

            cssLength.length_bits = decl->c_value->c_bg_pos_x;
            val_  = (double) cpp_cssLengthValue(cssLength);
            type_ = cpp_cssLengthType(cssLength);
            hll_computeDwLength(&attrs->backgroundPositionX, val_, type_, &attrs->font->font_attrs, layout->dpiX(), layout->dpiY());

            cssLength.length_bits = decl->c_value->c_bg_pos_y;
            val_  = (double) cpp_cssLengthValue(cssLength);
            type_ = cpp_cssLengthType(cssLength);
            hll_computeDwLength(&attrs->backgroundPositionY, val_, type_, &attrs->font->font_attrs, layout->dpiX(), layout->dpiY());

            break;
         case CSS_PROPERTY_BACKGROUND_REPEAT:
            attrs->backgroundRepeat = (BackgroundRepeat) decl->c_value->c_int_val;
            break;

-}

  -- Probably because of code like this someone invented lenses.
  | declaration == CssDeclarationBackgroundColor   = styleAttrs { styleBackgroundColor = getBackgroundColor value }
  | declaration == CssDeclarationBorderCollapse    = styleAttrs { styleBorderCollapse  = getBorderCollapse value }
  | declaration == CssDeclarationBorderTopStyle    = styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleTop    = getBorderStyle value }}
  | declaration == CssDeclarationBorderRightStyle  = styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleRight  = getBorderStyle value }}
  | declaration == CssDeclarationBorderBottomStyle = styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleBottom = getBorderStyle value }}
  | declaration == CssDeclarationBorderLeftStyle   = styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleLeft   = getBorderStyle value }}
  | declaration == CssDeclarationBorderTopWidth    = styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthTop    = getBorderWidth value dpiX dpiY fontAttrs }}
  | declaration == CssDeclarationBorderRightWidth  = styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthRight  = getBorderWidth value dpiX dpiY fontAttrs }}
  | declaration == CssDeclarationBorderBottomWidth = styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthBottom = getBorderWidth value dpiX dpiY fontAttrs }}
  | declaration == CssDeclarationBorderLeftWidth   = styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthLeft   = getBorderWidth value dpiX dpiY fontAttrs }}
  | declaration == CssDeclarationBorderTopColor    = styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorTop    = getBorderColor value }}
  | declaration == CssDeclarationBorderRightColor  = styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorRight  = getBorderColor value }}
  | declaration == CssDeclarationBorderBottomColor = styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorBottom = getBorderColor value }}
  | declaration == CssDeclarationBorderLeftColor   = styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorLeft   = getBorderColor value }}
  | declaration == CssDeclarationMarginBottom   = styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginBottom = getMargin distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationMarginLeft     = styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginLeft   = getMargin distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationMarginRight    = styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginRight  = getMargin distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationMarginTop      = styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginTop    = getMargin distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationPaddingBottom  = styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingBottom = getPadding distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationPaddingLeft    = styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingLeft   = getPadding distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationPaddingRight   = styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingRight  = getPadding distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationPaddingTop     = styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingTop    = getPadding distance fontAttrs dpiX dpiY }}
  | declaration == CssDeclarationTextAlign      = styleAttrs { styleTextAlign      = getTextAlign value }
  | declaration == CssDeclarationTextDecoration = styleAttrs { styleTextDecoration = getTextDecoration value (styleTextDecoration styleAttrs) }
  | declaration == CssDeclarationTextIndent     = styleAttrs { styleTextIndent     = getTextIndent distance fontAttrs dpiX dpiY }
  | declaration == CssDeclarationTextTransform  = styleAttrs { styleTextTransform  = getTextTransform value }
  | declaration == CssDeclarationVerticalAlign  = styleAttrs { styleVerticalAlign  = getVerticalAlign value }
  | declaration == CssDeclarationWhitespace     = styleAttrs { styleWhiteSpace     = getWhiteSpace value }
  | declaration == CssDeclarationWidth          = styleAttrs { styleWidth          = getWidthOrHeight distance fontAttrs dpiX dpiY }
  | declaration == CssDeclarationHeight         = styleAttrs { styleHeight         = getWidthOrHeight distance fontAttrs dpiX dpiY }
  | declaration == CssDeclarationListStylePosition = styleAttrs { styleListStylePosition    = getListStylePosition value }
  | declaration == CssDeclarationListStyleType     = styleAttrs { styleListStyleType        = getListStyleType value }
  | declaration == CssDeclarationLineHeight        = styleAttrs { styleLineHeight           = getLineHeight value distance fontAttrs dpiX dpiY }
  | declaration == CssDeclarationDisplay           = styleAttrs { styleDisplay              = getDisplay value }
  | declaration == CssDeclarationColor             = styleAttrs { styleColor                = getColor value }
  | declaration == CssDeclarationCursor            = styleAttrs { styleCursor               = getCursor value }
  | declaration == CssDeclarationBorderSpacing     = styleAttrs { styleHBorderSpacing = getBorderSpacing distance fontAttrs dpiX dpiY, styleVBorderSpacing = getBorderSpacing distance fontAttrs dpiX dpiY }
  | declaration == CssDeclarationWordSpacing       = styleAttrs { styleWordSpacing    = getWordSpacig value distance fontAttrs dpiX dpiY }
  | declaration == CssDeclarationXLink             = styleAttrs { styleXLink          = getXLink value }
  | declaration == CssDeclarationXLang             = styleAttrs { styleXLang          = getXLang value }
  | declaration == CssDeclarationXImg              = styleAttrs { styleXImg           = getXImg value }
  | declaration == CssDeclarationXTooltip          = styleAttrs { styleXTooltip       = getXTooltip value }
  | otherwise                                    = styleAttrs
    -- TODO: add support for missing cases

  where
    fontAttrs = styleFontAttrs styleAttrs
    distance = case value of
                 CssValueTypeLengthPercent d       -> d
                 CssValueTypeLength d              -> d
                 CssValueTypeSignedLength d        -> d
                 CssValueTypeLengthPercentNumber d -> d
                 CssValueTypeAuto d                -> d  -- TODO: 'auto' appears to be handled incorrectly this function
                 otherwise                         -> CssNumericAuto 0 -- TODO: I'm not sure if this is the best 'otherwise' value




getBorderCollapse value = case value of
                         CssValueTypeEnum i -> i
                         otherwise          -> 0




getBorderStyle value = case value of
                         CssValueTypeEnum i -> i
                         otherwise          -> 0




getBorderWidth value dpiX dpiY fontAttrs = case styleEngineComputeBorderWidth value dpiX dpiY fontAttrs of
                                             -- TODO: another place where Maybe returned by Compute function
                                             -- causes unnecessary trouble.
                                             Just x  -> x
                                             Nothing -> 0




getBorderColor value = case value of
                         CssValueTypeEnum e  -> 0x000000 -- Handling of enum keywords for border color is not implemented yet. Fall back to black. TODO: implement the enum.
                         CssValueTypeColor c -> c
                         otherwise           -> (-1) -- '-1' will be caught in temporary workaround in C++ code.




getTextAlign value = case value of
                       CssValueTypeEnum e -> e
                       otherwise          -> 0




getTextDecoration value decoration = decoration .|. case value of
                                                      CssValueTypeMultiEnum e -> e
                                                      otherwise               -> 0




getTextIndent distance fontAttrs dpiX dpiY =
  case styleEngineCalculateDwLength distance fontAttrs dpiX dpiY of
    Just length -> length
    Nothing     -> createAbsoluteDwLength 0 -- "0" seems to be a sane default




getTextTransform value = case value of
                           CssValueTypeEnum e -> e
                           otherwise          -> 0




getMargin distance fontAttrs dpiX dpiY = clip . calculate $ distance
  where
    calculate dist = case styleEngineComputeAbsoluteLengthValue dist fontAttrs 0 dpiX dpiY of
                       -- TODO: another place where Maybe returned by Compute function
                       -- causes unnecessary trouble.
                       Just x  -> roundInt x
                       Nothing -> 0
    clip x = if x > 0 then x else 0   -- TODO: fix negative margins in dw/*




getPadding distance fontAttrs dpiX dpiY =
  case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY of
    -- TODO: another place where Maybe returned by Compute function
    -- causes unnecessary trouble.
    Just x  -> roundInt x
    Nothing -> 0




getVerticalAlign value = case value of
                           CssValueTypeEnum e -> e
                           otherwise          -> 3 -- '3' corresponds to "vertical-align: baseline"




getWhiteSpace value = case value of
                           CssValueTypeEnum e -> e
                           otherwise          -> 0 -- '0' corresponds to "white-space: normal"



getWidthOrHeight distance fontAttrs dpiX dpiY =
  case styleEngineCalculateDwLength distance fontAttrs dpiX dpiY of
    Just length -> length
    Nothing     -> createPercentageDwLength 100 -- "100%" seems to be a sane default; TODO: is it really




getListStylePosition value = case value of
                               CssValueTypeEnum e -> e
                               otherwise          -> 0 -- 'o' corresponds to "list-style-position: inside"




getListStyleType value = case value of
                           CssValueTypeEnum e -> e
                           otherwise          -> 0 -- '0' corresponds to "list-style-type: disc"





getLineHeight value distance fontAttrs dpiX dpiY =
  case value of
    CssValueTypeEnum _                -> createAutoDwLength -- only valid enum value is "normal"
    CssValueTypeLengthPercentNumber d -> case d of -- TODO: 'd' duplicates 'distance' function arg
                                           CssNumericNone f -> createPercentageDwLength . realToFrac $ f
                                           otherwise        -> case styleEngineComputeAbsoluteLengthValue distance fontAttrs referenceValue dpiX dpiY of
                                                                 Just len -> createAbsoluteDwLength . roundInt $ len
                                                                 Nothing  -> createAutoDwLength -- TODO: is it the best choice?
    otherwise                         -> createAutoDwLength -- TODO: is it safe default?

    where
      referenceValue = fontSize fontAttrs

{-
         case CSS_PROPERTY_LINE_HEIGHT:
            if (decl->c_value->c_type_tag == CssDeclarationValueTypeENUM) { //only valid enum value is "normal"
               attrs->lineHeight = createAutoLength();
            } else if (decl->c_value->c_type_tag == CssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER) {

               int lineHeight;
               CssLength cssLength = cpp_cssCreateLength(decl->c_value->c_length_val, (CssLengthType) decl->c_value->c_length_type);
               if (cpp_cssLengthType(cssLength) == CSS_LENGTH_TYPE_NONE) {
                  attrs->lineHeight = createPercentageDwLength(cpp_cssLengthValue(cssLength));
               } else if ((bool) hll_styleEngineComputeAbsoluteLengthValue(cpp_cssLengthValue(cssLength), cpp_cssLengthType(cssLength), &attrs->font->font_attrs, attrs->font->font_attrs.size, layout->dpiX(), layout->dpiY(), &lineHeight)) {
                  attrs->lineHeight = createAbsoluteDwLength(lineHeight);
               }
            }
            break;
-}




getDisplay value = case value of
                     CssValueTypeEnum e -> e
                     otherwise          -> 1 -- '1' corresponds to "display: inline"; TODO: is it the best fallback value?




getColor value = case value of
                   CssValueTypeColor c -> c
                   otherwise           -> 0x000000 -- Black




getBackgroundColor value = case value of
                             CssValueTypeColor c -> c
                             otherwise           -> 0xffffff -- White




getCursor value = case value of
                    CssValueTypeEnum e -> e
                    otherwise          -> 1 -- '1' corresponds to "cursor: default"



-- TODO: border spacing uses the same value for H and V border spacing. If
-- CSS file specifies two separate values for H and V, the second one is
-- ignored.
getBorderSpacing distance fontAttrs dpiX dpiY =
  case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY of
    Just val -> round val -- TODO: a type of Float -> Int function to be verified here
    Nothing  -> 0         -- TODO: is it a good default?





css_WORD_SPACING_NORMAL = 0

getWordSpacig :: CssValue -> CssDistance -> FontAttrs -> Float -> Float -> Int
getWordSpacig value distance fontAttrs dpiX dpiY = clipSpacing (getSpacing value distance fontAttrs dpiX dpiY)

  where
    getSpacing :: CssValue -> CssDistance -> FontAttrs -> Float -> Float -> Int
    getSpacing value distance fontAttrs dpiX dpiY =
      case value of
        CssValueTypeEnum css_WORD_SPACING_NORMAL -> 0
        CssValueTypeEnum _                       -> 0 -- TODO: implement remaining enum values
        otherwise                                ->  case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 dpiX dpiY of
                                                       Just val -> round val -- TODO: a type of Float -> Int function to be verified here
                                                       Nothing  -> 0         -- TODO: is it a good default?

    -- Limit to reasonable values to avoid overflows
    clipSpacing :: Int -> Int
    clipSpacing s | s > 1000  = 1000
                  | s < -1000 = -1000
                  | otherwise = s




getXLink value = case value of
                   CssValueTypeInt i -> i
                   otherwise         -> 0




getXLang value = case value of
                   CssValueTypeString s -> if T.length s == 2 -- Only two-letter values of 'lang' attribute are allowed. TODO: is style engine the best place to validate the length?
                                           then T.toLower s
                                           else ""
                   otherwise            -> ""




getXImg value = case value of
                  CssValueTypeInt i -> i
                  otherwise         -> 0




getXTooltip value = case value of
                      CssValueTypeString s -> s
                      otherwise            -> ""




-- Inherit non-CSS hints from current element's parent to current element.
--
-- TODO: check order of arguments to 'append' function. Comment in C++ says
-- "original declListNonCss have precedence", which suggests that current
-- element's hints should have precenence.
--
-- The order is most probably correct because 'current' will overwrite
-- (update) any existing declarations in 'inherited', so 'current' will have
-- precedence.
--
-- TODO: there are at least two ways to improve this function:
--
-- 1. Get rid of Maybe. Non-existent declarations of 'current' will be
-- indicated by an empty 'current' set.
--
-- 2. Consider just calling 'declarationsSetAppend parent current'. It will
-- work well if current is empty or non-empty, and if parent is empty or
-- non-empty.
styleEngineInheritNonCssHints :: CssDeclarationSet -> Maybe CssDeclarationSet -> CssDeclarationSet
styleEngineInheritNonCssHints parent mCurrent = inheritedAndCurrent
  where
    inheritedAndCurrent = case mCurrent of
                            Just current -> declarationsSetAppend inherited current
                            Nothing      -> inherited
    inherited = parent -- "copy constructor"



