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

  , FontAttrs (..)
  , defaultFontAttrs

  , styleEngineApplyStyleToFont

  , styleEngineSetFontFamily
  , styleEngineSetFontWeight
  , styleEngineSetFontSize
  , styleEngineSetFontSize'
  , styleEngineSetFontStyle
  , styleEngineSetLetterSpacing
  , styleEngineSetFontVariant
  )
where




import Prelude
import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T
import Debug.Trace

import Hello.Css.Distance
import Hello.Css.Parser

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




css_FONT_WEIGHT_BOLD    = 0
css_FONT_WEIGHT_BOLDER  = 1
css_FONT_WEIGHT_LIGHT   = 2
css_FONT_WEIGHT_LIGHTER = 3
css_FONT_WEIGHT_NORMAL  = 4




-- TODO: this type probably should go elsewhere.
data FontAttrs = FontAttrs
  {
    fontSize          :: Int
  , fontWeight        :: Int
  , fontName          :: T.Text
  , fontVariant       :: Int
  , fontStyle         :: Int

  , fontXHeight       :: Int
  , fontLetterSpacing :: Int
  } deriving (Show, Eq)




defaultFontAttrs = FontAttrs
  {
    fontSize    = 0
  , fontWeight  = 0
  , fontName    = ""
  , fontVariant = 0
  , fontStyle   = 0

  , fontXHeight       = 0
  , fontLetterSpacing = 0

  }




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
    apply :: S.Seq CssDeclaration -> Preferences -> Float -> Float -> FontAttrs -> FontAttrs -> FontAttrs
    apply decls prefs dpiX dpiY parentFontAttrs fontAttrs =
      case S.null decls of
        True -> fontAttrs
        False  | property x == cssDeclPropertyFontFamily    {- 32 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontFamily value prefs fontAttrs
               | property x == cssDeclPropertyFontSize      {- 33 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontSize' value prefs dpiX dpiY parentFontAttrs fontAttrs
               | property x == cssDeclPropertyFontStyle     {- 36 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontStyle value fontAttrs
               | property x == cssDeclPropertyFontVariant   {- 37 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontVariant value fontAttrs
               | property x == cssDeclPropertyFontWeight    {- 38 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetFontWeight value fontAttrs
               | property x == cssDeclPropertyLetterSpacing {- 41 -} -> apply xs prefs dpiX dpiY parentFontAttrs $ styleEngineSetLetterSpacing value dpiX dpiY parentFontAttrs fontAttrs
               | otherwise                                  -> apply xs prefs dpiX dpiY parentFontAttrs $ fontAttrs
          where
            x  :: CssDeclaration       = S.index decls 0
            xs :: S.Seq CssDeclaration = S.drop 1 decls
            value :: CssValue = declValue x


