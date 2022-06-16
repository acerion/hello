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




{-# LANGUAGE OverloadedStrings #-}




module Hello.Css.StyleEngine
  (
    styleEngineComputeAbsoluteLengthValue

  , FontAttrs (..)
  , defaultFontAttrs
  , styleEngineSetFontWeight
  )
where




import Prelude
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Debug.Trace

import Hello.Css.Distance
import Hello.Css.Parser

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
  } deriving (Show)




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




-- https://www.w3schools.com/cssref/pr_font_weight.asp
-- https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight
styleEngineSetFontWeight :: CssValue -> FontAttrs -> Maybe FontAttrs
styleEngineSetFontWeight value attrs = clipWeight . setWeight $ attrs
  where
    setWeight attrs = case value of
                        CssValueTypeEnum       i -> byEnum attrs i
                        -- Per Parser.hs, a special type for weight. TODO: do we need this type?
                        CssValueTypeFontWeight i -> Just attrs { fontWeight = i }
                        otherwise                -> Nothing

    clipWeight = fmap clipFunction

    -- TODO: the limit may be 1000, not 900.
    clipFunction a | fontWeight a < 100 = a { fontWeight = 100 }
                   | fontWeight a > 900 = a { fontWeight = 900 }
                   | otherwise          = a

    byEnum attrs i | i == css_FONT_WEIGHT_BOLD    = Just $ attrs { fontWeight = 700 }
                   | i == css_FONT_WEIGHT_BOLDER  = Just $ attrs { fontWeight = (fontWeight attrs) + 300 }
                   | i == css_FONT_WEIGHT_LIGHT   = Just $ attrs { fontWeight = 100 }
                   | i == css_FONT_WEIGHT_LIGHTER = Just $ attrs { fontWeight = (fontWeight attrs) - 300 }
                   | i == css_FONT_WEIGHT_NORMAL  = Just $ attrs { fontWeight = 400 }
                   | otherwise                    = Nothing


