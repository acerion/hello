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
  -- , styleEngineSetFontSize'
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
import qualified Data.Sequence as S
import qualified Data.Text as T
--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.Parser.Rule

import Hello.Display

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
styleEngineComputeAbsoluteLengthValue :: CssDistance -> FontAttrs -> Int -> Display -> Maybe Float
styleEngineComputeAbsoluteLengthValue distance fontAttrs referenceValue display =
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
    CssNumericPercentage d -> Just (fromIntegral (roundInt (d * fromIntegral referenceValue)))
    CssNumericRelative _   -> Nothing
    CssDistanceAuto        -> Nothing

  where
    -- Assume dpiX == dpiY
    --
    -- TODO: is this assumption always correct? Data in
    -- src/Hello/Tests/Css/StyleEngine.hs shows that it's not correct.
    dpmm = dpiX display / 25.4




-- https://www.w3schools.com/cssref/pr_font_font-family.asp
-- https://developer.mozilla.org/pl/docs/Web/CSS/font-family
-- https://www.w3.org/TR/CSS22/fonts.html#font-family-prop
styleEngineSetFontFamily :: CssValueFontFamily -> Preferences -> FontAttrs -> FontAttrs
styleEngineSetFontFamily (CssValueFontFamilyList l) preferences fontAttrsArg = setName l preferences fontAttrsArg
  where
    setName (x:xs) prefs fontAttrs | x == "serif"      = fontAttrs { fontName = prefsFontSerif prefs }
                                   | x == "sans-serif" = fontAttrs { fontName = prefsFontSansSerif prefs }
                                   | x == "cursive"    = fontAttrs { fontName = prefsFontCursive prefs }
                                   | x == "fantasy"    = fontAttrs { fontName = prefsFontFantasy prefs }
                                   | x == "monospace"  = fontAttrs { fontName = prefsFontMonospace prefs }
                                   | fontExists x      = fontAttrs { fontName = x }
                                   | otherwise         = setName xs prefs fontAttrs
    setName [] _ fontAttrs                             = fontAttrs

    -- TODO: implement lookup of font name in Operating System. In dillo this
    -- has been done through Font::exists(layout, c_value->c_text_val).
    --
    -- For now this program doesn't access list of fonts available on OS, so
    -- fontExists must always return False.
    fontExists _ = False




styleEngineSetFontWeight :: CssValueFontWeight -> FontAttrs -> FontAttrs
styleEngineSetFontWeight declValue fontAttrs = clipWeight $ setWeight declValue fontAttrs
  where
    setWeight CssValueFontWeightNormal  attrs = attrs { fontWeight = 400 }
    setWeight CssValueFontWeightBold    attrs = attrs { fontWeight = 700 }
    setWeight CssValueFontWeightBolder  attrs = attrs { fontWeight = fontWeight attrs + 300 }
    setWeight CssValueFontWeightLighter attrs = attrs { fontWeight = fontWeight attrs - 300 }
    setWeight (CssValueFontWeightInt i) attrs = attrs { fontWeight = i }

    -- TODO: the limit may be 1000, not 900.
    --
    -- Even though interpretTokensAsInteger ensures that only values in
    -- range 100-900 are accepted, a clipping is still necessary: if
    -- calculations done for 'bolder' or 'lighter' result in values out of
    -- the range, the calculated values must be clipped.
    clipWeight a | fontWeight a < 100 = a { fontWeight = 100 }
                 | fontWeight a > 900 = a { fontWeight = 900 }
                 | otherwise          = a





{-
css_FONT_SIZE_LARGE    = 0
css_FONT_SIZE_LARGER   = 1
css_FONT_SIZE_MEDIUM   = 2
css_FONT_SIZE_SMALL    = 3
css_FONT_SIZE_SMALLER  = 4
css_FONT_SIZE_XX_LARGE = 5
css_FONT_SIZE_XX_SMALL = 6
css_FONT_SIZE_X_LARGE  = 7
css_FONT_SIZE_X_SMALL  = 8




styleEngineSetFontSize' :: CssValueFontSize -> Preferences -> Display -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineSetFontSize' declValue prefs display parentFontAttrs fontAttrs = styleEngineSetFontSize input prefs display parentFontAttrs fontAttrs
  where
    input = case value of
              CssValueTypeEnum enum              -> integerToFontSize enum
              CssValueTypeLengthPercent distance -> Just $ CssValueFontSize distance

    integerToFontSize e | e == css_FONT_SIZE_XX_SMALL = Just CssValueFontSizeXXSmall
                        | e == css_FONT_SIZE_X_SMALL  = Just CssValueFontSizeXSmall
                        | e == css_FONT_SIZE_SMALL    = Just CssValueFontSizeSmall
                        | e == css_FONT_SIZE_MEDIUM   = Just CssValueFontSizeMedium
                        | e == css_FONT_SIZE_LARGE    = Just CssValueFontSizeLarge
                        | e == css_FONT_SIZE_X_LARGE  = Just CssValueFontSizeXLarge
                        | e == css_FONT_SIZE_XX_LARGE = Just CssValueFontSizeXXLarge
                        | e == css_FONT_SIZE_SMALLER  = Just CssValueFontSizeSmaller
                        | e == css_FONT_SIZE_LARGER   = Just CssValueFontSizeLarger
                        | otherwise                   = Nothing
-}




-- https://developer.mozilla.org/pl/docs/Web/CSS/font-size
-- https://www.w3schools.com/cssref/pr_font_font-size.asp
styleEngineSetFontSize :: CssValueFontSize -> Preferences -> Display -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineSetFontSize declValueArg prefs display parentFontAttrs fontAttrsArg = clipSize (setAbsSize declValueArg fontAttrsArg)
  where
    setAbsSize :: CssValueFontSize -> FontAttrs -> FontAttrs
    setAbsSize declValue fontAttrs = case fontSizeToAbs declValue prefs display fontAttrs parentFontAttrs of
                                       Just size -> fontAttrs { fontSize = size }
                                       Nothing   -> fontAttrs

    clipSize :: FontAttrs -> FontAttrs
    clipSize a | fontSize a < prefsFontMinSize prefs = a { fontSize = prefsFontMinSize prefs }
               | fontSize a > prefsFontMaxSize prefs = a { fontSize = prefsFontMaxSize prefs }
               | otherwise                           = a




fontSizeToAbs :: CssValueFontSize -> Preferences -> Display -> FontAttrs -> FontAttrs -> Maybe Int
fontSizeToAbs declValue prefs display fontAttrs parentFontAttrs = case declValue of
                                                                    CssValueFontSizeXXSmall   -> Just $ roundInt ( 8.1  * prefsFontFactor prefs)
                                                                    CssValueFontSizeXSmall    -> Just $ roundInt ( 9.7  * prefsFontFactor prefs)
                                                                    CssValueFontSizeSmall     -> Just $ roundInt (11.7  * prefsFontFactor prefs)
                                                                    CssValueFontSizeMedium    -> Just $ roundInt (14.0  * prefsFontFactor prefs)
                                                                    CssValueFontSizeLarge     -> Just $ roundInt (16.8  * prefsFontFactor prefs)
                                                                    CssValueFontSizeXLarge    -> Just $ roundInt (20.2  * prefsFontFactor prefs)
                                                                    CssValueFontSizeXXLarge   -> Just $ roundInt (24.2  * prefsFontFactor prefs)
                                                                    CssValueFontSizeLarger    -> Just $ roundInt ( 1.2  * (fromIntegral . fontSize $ fontAttrs))
                                                                    CssValueFontSizeSmaller   -> Just $ roundInt ( 0.83 * (fromIntegral . fontSize $ fontAttrs))
                                                                    CssValueFontSizeDistance distance -> case size of
                                                                                                           Just s  -> Just $ roundInt s
                                                                                                           Nothing -> Nothing
                                                                      where
                                                                        size           = styleEngineComputeAbsoluteLengthValue distance parentFontAttrs referenceValue display
                                                                        referenceValue = fontSize parentFontAttrs




-- https://developer.mozilla.org/pl/docs/Web/CSS/font-style
-- https://www.w3schools.com/cssref/pr_font_font-style.asp
--
-- Translate value of "font-style" property from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when you finally add support for other (non-enum) values
-- of the property, you won't be able to use fromEnum anymore. The new values
-- will complicate the function.
styleEngineSetFontStyle :: CssValueFontStyle -> FontAttrs -> FontAttrs
styleEngineSetFontStyle declValue fontAttrs = fontAttrs { fontStyle = fromEnum declValue }




styleEngineSetLetterSpacing :: CssValueLetterSpacing -> Display -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineSetLetterSpacing declValue display parentFontAttrs fontAttrsArg = clipSpacing $ setSpacing declValue fontAttrsArg
  where
    setSpacing :: CssValueLetterSpacing -> FontAttrs -> FontAttrs
    setSpacing CssValueLetterSpacingNormal              fontAttrs = fontAttrs { fontLetterSpacing = 0 }
    setSpacing (CssValueLetterSpacingDistance distance) fontAttrs =
      case size of
        Just s  -> fontAttrs { fontLetterSpacing = roundInt s }
        Nothing -> fontAttrs
      where
        size           = styleEngineComputeAbsoluteLengthValue distance parentFontAttrs referenceValue display
        referenceValue = fontSize parentFontAttrs


    --Limit letterSpacing to reasonable values to avoid overflows e.g, when
    --measuring word width.
    clipSpacing :: FontAttrs -> FontAttrs
    clipSpacing a | fontLetterSpacing a < -1000 = a { fontLetterSpacing = -1000 }
                  | fontLetterSpacing a >  1000 = a { fontLetterSpacing =  1000 }
                  | otherwise                   = a




-- https://www.w3schools.com/cssref/pr_font_font-variant.asp
--
-- Translate value of "list-style-position" from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
styleEngineSetFontVariant :: CssValueFontVariant -> FontAttrs -> FontAttrs
styleEngineSetFontVariant declValue fontAttrs = fontAttrs { fontVariant = fromEnum declValue }




styleEngineApplyStyleToFont :: CssDeclarationSet -> Preferences -> Display -> FontAttrs -> FontAttrs -> FontAttrs
styleEngineApplyStyleToFont declSet preferences displayArg parentFontAttrsArg fontAttrsArg = apply (items declSet) preferences displayArg parentFontAttrsArg fontAttrsArg
  where
    apply :: S.Seq CssDeclaration -> Preferences -> Display -> FontAttrs -> FontAttrs -> FontAttrs
    apply decls prefs display parentFontAttrs fontAttrs =
      case S.null decls of
        True -> fontAttrs
        False -> case property x of
                   CssPropertyFontFamily value        -> apply xs prefs display parentFontAttrs $ styleEngineSetFontFamily value prefs fontAttrs
                   CssPropertyFontSize declValue      -> apply xs prefs display parentFontAttrs $ styleEngineSetFontSize declValue prefs display parentFontAttrs fontAttrs
                   CssPropertyFontStyle declValue     -> apply xs prefs display parentFontAttrs $ styleEngineSetFontStyle declValue fontAttrs
                   CssPropertyFontVariant declValue   -> apply xs prefs display parentFontAttrs $ styleEngineSetFontVariant declValue fontAttrs
                   CssPropertyFontWeight declValue    -> apply xs prefs display parentFontAttrs $ styleEngineSetFontWeight declValue fontAttrs
                   CssPropertyLetterSpacing declValue -> apply xs prefs display parentFontAttrs $ styleEngineSetLetterSpacing declValue display parentFontAttrs fontAttrs
                   _                                  -> apply xs prefs display parentFontAttrs fontAttrs
          where
            x  :: CssDeclaration       = S.index decls 0
            xs :: S.Seq CssDeclaration = S.drop 1 decls




styleEngineCalculateDwLength :: CssDistance -> FontAttrs -> Display -> Maybe DwLength
styleEngineCalculateDwLength distance fontAttrs display =
  case distance of
    CssNumericPercentage v -> Just $ createPercentageDwLength (realToFrac v)
    CssDistanceAuto        -> Just createAutoDwLength
    _                      -> case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 display of
                                Just val -> Just $ createAbsoluteDwLength (round val) -- TODO: a type of Float -> Int function to be verified here
                                Nothing  -> Nothing




styleEngineApplyStyleToGivenNode :: CssDeclarationSet -> Preferences -> Display -> StyleAttrs -> StyleAttrs -> StyleAttrs
styleEngineApplyStyleToGivenNode declSet prefs display parentStyleAttrs styleAttrs = styleAttrs'
  where
    -- Determine font first so it can be used to resolve relative lengths of other elements.
    fontAttrs'  = styleEngineApplyStyleToFont declSet prefs display (styleFontAttrs parentStyleAttrs) (styleFontAttrs styleAttrs)
    styleAttrs' = setRemainingAttrs (items declSet) display styleAttrs { styleFontAttrs = fontAttrs' }

    setRemainingAttrs :: S.Seq CssDeclaration -> Display -> StyleAttrs -> StyleAttrs
    setRemainingAttrs decls disp styleAttrsArg =
      case S.null decls of
        True  -> styleAttrsArg
        False -> setRemainingAttrs xs disp $ styleEngineSetStyle (property x) display parentStyleAttrs styleAttrsArg
          where
            x  :: CssDeclaration       = S.index decls 0
            xs :: S.Seq CssDeclaration = S.drop 1 decls




styleEngineSetStyle :: CssProperty -> Display -> StyleAttrs -> StyleAttrs -> StyleAttrs
styleEngineSetStyle declaration display parentStyleAttrs styleAttrs =
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
  case declaration of
    CssPropertyBackgroundColor declValue     -> styleAttrs { styleBackgroundColor = getBackgroundColor parentStyleAttrs declValue }
    CssPropertyBorderCollapse declValue      -> styleAttrs { styleBorderCollapse  = getBorderCollapse declValue }

    CssPropertyBorderTopStyle value    -> styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleTop    = getBorderStyleTop    parentStyleAttrs value }}
    CssPropertyBorderRightStyle value  -> styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleRight  = getBorderStyleRight  parentStyleAttrs value }}
    CssPropertyBorderBottomStyle value -> styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleBottom = getBorderStyleBottom parentStyleAttrs value }}
    CssPropertyBorderLeftStyle value   -> styleAttrs { styleBorderStyle = (styleBorderStyle styleAttrs) { styleBorderStyleLeft   = getBorderStyleLeft   parentStyleAttrs value }}

    CssPropertyBorderTopWidth value    -> styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthTop    = getBorderWidthTop    parentStyleAttrs value display fontAttrs }}
    CssPropertyBorderRightWidth value  -> styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthRight  = getBorderWidthRight  parentStyleAttrs value display fontAttrs }}
    CssPropertyBorderBottomWidth value -> styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthBottom = getBorderWidthBottom parentStyleAttrs value display fontAttrs }}
    CssPropertyBorderLeftWidth value   -> styleAttrs { styleBorderWidth = (styleBorderWidth styleAttrs) { styleBorderWidthLeft   = getBorderWidthLeft   parentStyleAttrs value display fontAttrs }}

    CssPropertyBorderTopColor value    -> styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorTop    = getBorderColorTop    parentStyleAttrs value }}
    CssPropertyBorderRightColor value  -> styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorRight  = getBorderColorRight  parentStyleAttrs value }}
    CssPropertyBorderBottomColor value -> styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorBottom = getBorderColorBottom parentStyleAttrs value }}
    CssPropertyBorderLeftColor value   -> styleAttrs { styleBorderColor = (styleBorderColor styleAttrs) { styleBorderColorLeft   = getBorderColorLeft   parentStyleAttrs value }}

    CssPropertyMargin declValue        -> styleAttrs { styleMargin  = updateStyleMargin declValue (styleMargin styleAttrs) fontAttrs display }
    CssPropertyMarginTop declValue     -> styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginTop    = getMargin declValue fontAttrs display }}
    CssPropertyMarginRight declValue   -> styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginRight  = getMargin declValue fontAttrs display }}
    CssPropertyMarginBottom declValue  -> styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginBottom = getMargin declValue fontAttrs display }}
    CssPropertyMarginLeft declValue    -> styleAttrs { styleMargin  = (styleMargin styleAttrs) { styleMarginLeft   = getMargin declValue fontAttrs display }}

    CssPropertyPaddingTop declValue    -> styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingTop    = getPadding declValue fontAttrs display }}
    CssPropertyPaddingRight declValue  -> styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingRight  = getPadding declValue fontAttrs display }}
    CssPropertyPaddingBottom declValue -> styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingBottom = getPadding declValue fontAttrs display }}
    CssPropertyPaddingLeft declValue   -> styleAttrs { stylePadding = (stylePadding styleAttrs) { stylePaddingLeft   = getPadding declValue fontAttrs display }}

    CssPropertyTextAlign declValue     -> styleAttrs { styleTextAlign      = getTextAlign declValue }
    CssPropertyTextDecoration declValue -> styleAttrs { styleTextDecoration = getTextDecoration declValue (styleTextDecoration styleAttrs) }
    CssPropertyTextIndent declValue    -> styleAttrs { styleTextIndent     = getTextIndent declValue fontAttrs display }
    CssPropertyTextTransform declValue -> styleAttrs { styleTextTransform  = getTextTransform declValue }
    CssPropertyVerticalAlign value     -> styleAttrs { styleVerticalAlign  = getVerticalAlign value }
    CssPropertyWhitespace value        -> styleAttrs { styleWhiteSpace     = getWhiteSpace value }
    CssPropertyWidth declValue         -> styleAttrs { styleWidth          = getWidth declValue fontAttrs display }
    CssPropertyHeight declValue        -> styleAttrs { styleHeight         = getHeight declValue fontAttrs display }
    CssPropertyListStylePosition value -> styleAttrs { styleListStylePosition    = getListStylePosition value }
    CssPropertyListStyleType value     -> styleAttrs { styleListStyleType        = getListStyleType value }
    CssPropertyLineHeight declValue    -> styleAttrs { styleLineHeight           = getLineHeight declValue fontAttrs display }
    CssPropertyDisplay value           -> styleAttrs { styleDisplay              = getDisplay value }
    CssPropertyColor value             -> styleAttrs { styleColor                = getColor parentStyleAttrs value }
    CssPropertyCursor value            -> styleAttrs { styleCursor               = getCursor value }
    CssPropertyBorderSpacing declValue -> styleAttrs { styleHBorderSpacing = getBorderSpacing declValue fontAttrs display,
                                                          styleVBorderSpacing = getBorderSpacing declValue fontAttrs display }
    CssPropertyWordSpacing declValue   -> styleAttrs { styleWordSpacing    = getWordSpacig declValue fontAttrs display }
    CssPropertyXLink declValue         -> styleAttrs { styleXLink          = getXLink declValue }
    CssPropertyXLang declValue         -> styleAttrs { styleXLang          = getXLang declValue }
    CssPropertyXImg declValue          -> styleAttrs { styleXImg           = getXImg declValue }
    CssPropertyXTooltip declValue      -> styleAttrs { styleXTooltip       = getXTooltip declValue }
    _                                  -> styleAttrs
    -- TODO: add support for missing cases

  where
    fontAttrs = styleFontAttrs styleAttrs




-- Translate value of "border-collapse" declaration from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
getBorderCollapse :: CssValueBorderCollapse -> Int
getBorderCollapse declValue = fromEnum declValue




getBorderStyleTop    = getBorderStyle styleBorderStyleTop
getBorderStyleRight  = getBorderStyle styleBorderStyleRight
getBorderStyleBottom = getBorderStyle styleBorderStyleBottom
getBorderStyleLeft   = getBorderStyle styleBorderStyleLeft

getBorderStyle :: (StyleBorderStyle -> Int) -> StyleAttrs -> CssValueBorderStyle -> Int
getBorderStyle field parentStyleAttrs value = case value of
                                                -- These integer values are understood by C++ code.
                                                CssValueBorderStyleNone    -> 0
                                                CssValueBorderStyleHidden  -> 1
                                                CssValueBorderStyleDotted  -> 2
                                                CssValueBorderStyleDashed  -> 3
                                                CssValueBorderStyleSolid   -> 4
                                                CssValueBorderStyleDouble  -> 5
                                                CssValueBorderStyleGroove  -> 6
                                                CssValueBorderStyleRidge   -> 7
                                                CssValueBorderStyleInset   -> 8
                                                CssValueBorderStyleOutset  -> 9
                                                CssValueBorderStyleInherit -> field . styleBorderStyle $ parentStyleAttrs




getBorderWidthTop    = getBorderWidth styleBorderWidthTop
getBorderWidthRight  = getBorderWidth styleBorderWidthRight
getBorderWidthBottom = getBorderWidth styleBorderWidthBottom
getBorderWidthLeft   = getBorderWidth styleBorderWidthLeft

getBorderWidth field parentStyleAttrs declValue display fontAttrs = case declValue of
                                                                      CssValueBorderWidthInherit -> field . styleBorderWidth $ parentStyleAttrs
                                                                      CssValueBorderWidthThin       -> 1
                                                                      CssValueBorderWidthMedium     -> 2
                                                                      CssValueBorderWidthThick      -> 3
                                                                      CssValueBorderWidthDistance d -> case styleEngineComputeBorderWidth d display fontAttrs of
                                                                                                         -- TODO: another place where Maybe returned by Compute function
                                                                                                         -- causes unnecessary trouble.
                                                                                                         Just x  -> x
                                                                                                         Nothing -> 0
                                                                      -- TODO: otherwise is most probably unnecesary because
                                                                      -- the compiler will warn us about unhandled patterns.
                                                                      -- otherwise -> trace ("unknown value " ++ (show declValue)) (undefined)




-- TODO: re-think value returned by styleEngineComputeAbsoluteLengthValue: it
-- most probably should be Int, not Float. Then the Float->int conversion
-- won't be necessary.
styleEngineComputeBorderWidth :: CssDistance -> Display -> FontAttrs -> Maybe Int
styleEngineComputeBorderWidth distance display fontAttrs = roundInt <$> styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 display








getBorderColorTop    = getBorderColor styleBorderColorTop
getBorderColorRight  = getBorderColor styleBorderColorRight
getBorderColorBottom = getBorderColor styleBorderColorBottom
getBorderColorLeft   = getBorderColor styleBorderColorLeft

getBorderColor :: (StyleBorderColor -> Int) -> StyleAttrs -> CssValueBorderColor -> Int
getBorderColor field parentStyleAttrs value = case value of
                                                CssValueBorderColorInherit     -> field . styleBorderColor $ parentStyleAttrs
                                                CssValueBorderColorTransparent -> (-1) -- TODO: implement support for transparency; (-1) is a special value handled in C++ code for invalid colors.
                                                CssValueBorderColor i          -> i




-- Translate value of "text-align" property from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when you finally add support for other (non-enum) values
-- of the property, you won't be able to use fromEnum anymore. The new values
-- will complicate the function.
getTextAlign declValue = fromEnum declValue




-- TODO: this function could probably be rewritten as a fold.
getTextDecoration :: [CssValueTextDecoration] -> Int -> Int
getTextDecoration []     decoration = decoration
getTextDecoration (x:xs) decoration = decoration .|. getBit x .|. getTextDecoration xs decoration
  where
    getBit declValue = case declValue of
                         CssValueTextDecorationUnderline   -> 0x01
                         CssValueTextDecorationOverline    -> 0x02
                         CssValueTextDecorationLineThrough -> 0x04
                         CssValueTextDecorationBlink       -> 0x08




getTextIndent :: CssValueTextIndent -> FontAttrs -> Display -> DwLength
getTextIndent (CssValueTextIndentDistance distance) fontAttrs display =
  case styleEngineCalculateDwLength distance fontAttrs display of
    Just len -> len
    Nothing  -> createAbsoluteDwLength 0 -- "0" seems to be a sane default




-- Translate value of "text-transform" property from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when you finally add support for other (non-enum) values
-- of the property, you won't be able to use fromEnum anymore. The new values
-- will complicate the function.
getTextTransform declValue = fromEnum declValue




-- Update all four margins in given StyleMargin variable. Update them using
-- four values from CssValueMargin.
updateStyleMargin :: CssValueMargin -> StyleMargin -> FontAttrs -> Display -> StyleMargin
updateStyleMargin (CssValueMargin t r b l) style fontAttrs display =
  style { styleMarginTop    = clip . calculate $ t
        , styleMarginRight  = clip . calculate $ r
        , styleMarginBottom = clip . calculate $ b
        , styleMarginLeft   = clip . calculate $ l
        }
  where
    -- TODO: the calculate function is duplicated in getMargin. Remove duplication.
    calculate (CssValueMarginXDistance dist) =
      case styleEngineComputeAbsoluteLengthValue dist fontAttrs 0 display of
        -- TODO: another place where Maybe returned by Compute function
        -- causes unnecessary trouble.
        Just x  -> roundInt x
        Nothing -> 0
    clip x = if x > 0 then x else 0   -- TODO: fix negative margins in dw/*




getMargin :: CssValueMarginX -> FontAttrs -> Display -> Int
getMargin (CssValueMarginXDistance distance) fontAttrs display = clip . calculate $ distance
  where
    calculate dist = case styleEngineComputeAbsoluteLengthValue dist fontAttrs 0 display of
                       -- TODO: another place where Maybe returned by Compute function
                       -- causes unnecessary trouble.
                       Just x  -> roundInt x
                       Nothing -> 0
    clip x = if x > 0 then x else 0   -- TODO: fix negative margins in dw/*




getPadding :: CssValuePadding -> FontAttrs -> Display -> Int
getPadding (CssValuePadding distance) fontAttrs display =
  case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 display of
    -- TODO: another place where Maybe returned by Compute function
    -- causes unnecessary trouble.
    Just x  -> roundInt x
    Nothing -> 0




-- Translate value of "vertical-align" property from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when you finally add support for other (non-enum) values
-- of the property, you won't be able to use fromEnum anymore. The new values
-- will complicate the function.
getVerticalAlign declValue = fromEnum declValue




-- Translate value of "white-space" property from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
getWhiteSpace declValue = fromEnum declValue




getHeight :: CssValueHeight -> FontAttrs -> Display -> DwLength
getHeight (CssValueHeightDistance distance) fontAttrs display =
  case styleEngineCalculateDwLength distance fontAttrs display of
    Just len -> len
    Nothing  -> createPercentageDwLength 100 -- "100%" seems to be a sane default; TODO: is it really?




getWidth :: CssValueWidth -> FontAttrs -> Display -> DwLength
getWidth (CssValueWidthDistance distance) fontAttrs display =
  case styleEngineCalculateDwLength distance fontAttrs display of
    Just len -> len
    Nothing  -> createPercentageDwLength 100 -- "100%" seems to be a sane default; TODO: is it really?




-- Translate value of "list-style-position" from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
getListStylePosition :: CssValueListStylePosition -> Int
getListStylePosition declValue = fromEnum declValue




-- Translate value of "list-style-type" from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
getListStyleType :: CssValueListStyleType -> Int
getListStyleType declValue = fromEnum declValue




getLineHeight CssValueLineHeightNormal                        _         _       = createAutoDwLength
getLineHeight (CssValueLineHeightDistance (CssNumericNone f)) _         _       = createPercentageDwLength . realToFrac $ f
getLineHeight (CssValueLineHeightDistance distance)           fontAttrs display =
  case styleEngineComputeAbsoluteLengthValue distance fontAttrs referenceValue display of
    Just len -> createAbsoluteDwLength . roundInt $ len
    Nothing  -> createAutoDwLength -- TODO: is it the best choice?
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




-- Translate value of "display" declaration from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
getDisplay declValue = fromEnum declValue




getColor :: StyleAttrs -> CssValueColor -> Int
getColor parentStyleAttrs value = case value of
                                    CssValueColorInherit -> styleColor parentStyleAttrs
                                    CssValueColor i      -> i




getBackgroundColor :: StyleAttrs -> CssValueBackgroundColor -> Int
getBackgroundColor parentStyleAttrs declValue = case declValue of
                                                  CssValueBackgroundColorInherit -> styleBackgroundColor parentStyleAttrs
                                                  CssValueBackgroundColorColor i -> i




-- Translate value of "cursor" declaration from Haskell data into value
-- understood by C++ code.
--
-- TODO: notice that when adding support for "inherit" and "initial", you
-- won't be able to use fromEnum anymore. The two new values will complicate
-- the function.
getCursor :: CssValueCursor -> Int
getCursor declValue = fromEnum declValue




-- TODO: border spacing uses the same value for H and V border spacing. If
-- CSS file specifies two separate values for H and V, the second one is
-- ignored.
getBorderSpacing :: CssValueBorderSpacing -> FontAttrs -> Display -> Int
getBorderSpacing (CssValueBorderSpacingDistance distance) fontAttrs display =
  case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 display of
    Just val -> round val -- TODO: a type of Float -> Int function to be verified here
    Nothing  -> 0         -- TODO: is it a good default?




-- TODO: The spec
-- (https://www.w3.org/TR/CSS22/text.html#propdef-word-spacing) says that
-- specified word spacing value is "in addition to" default spacing. Make
-- sure that the implementation follows the spec. Does the "normal==0"
-- indicate that zero is default spacing, and a value specified in
-- declaration is added to the zero?
getWordSpacig :: CssValueWordSpacing -> FontAttrs -> Display -> Int
getWordSpacig declValue fontAttrsArg display = clipSpacing (getSpacing declValue fontAttrsArg display)
  where
    getSpacing :: CssValueWordSpacing -> FontAttrs -> Display -> Int
    getSpacing CssValueWordSpacingNormal              _         _    = 0
    getSpacing (CssValueWordSpacingDistance distance) fontAttrs disp =
      case styleEngineComputeAbsoluteLengthValue distance fontAttrs 0 disp of
        Just val -> round val -- TODO: a type of Float -> Int function to be verified here
        Nothing  -> 0         -- TODO: is it a good default?

    -- Limit to reasonable values to avoid overflows
    clipSpacing :: Int -> Int
    clipSpacing s | s > 1000  = 1000
                  | s < -1000 = -1000
                  | otherwise = s




-- Translate value of "x-link" pseudo-property from Haskell data into value
-- understood by C++ code.
getXLink :: CssValueXLink -> Int
getXLink (CssValueXLink i) = i




-- Only two-letter values of 'lang' attribute are allowed. TODO: is style
-- engine the best place to validate the length?
getXLang :: CssValueXLang -> T.Text
getXLang (CssValueXLang l) | T.length l == 2 = T.toLower l
                           | otherwise       = ""




-- Translate value of "x-img" pseudo-property from Haskell data into value
-- understood by C++ code.
getXImg :: CssValueXImg -> Int
getXImg (CssValueXImg i) = i




getXTooltip :: CssValueXTooltip -> T.Text
getXTooltip (CssValueXTooltip t) = t




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



