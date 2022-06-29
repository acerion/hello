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

This file is derived from dillo-3.0.5/dw/style.*
Copyright assignments from style.cc file:
Copyright 2005-2007 Sebastian Geerken <sgeerken@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Dw.Style
  (
    StyleAttrs (..)
  , StyleBorderStyle (..)
  , StyleBorderWidth (..)
  , StyleBorderColor (..)
  , StyleMargin (..)
  , StylePadding (..)
  )
  where




import Data.Text as T
import Debug.Trace

import Hello.Dw.DwLength
import Hello.Dw.FontAttrs




data StyleBorderStyle = StyleBorderStyle
  {
    styleBorderStyleTop    :: Int
  , styleBorderStyleRight  :: Int
  , styleBorderStyleBottom :: Int
  , styleBorderStyleLeft   :: Int
  } deriving (Show)




-- TODO: in dillo the borderWidth variables were of type Box. The comment for
-- Box type was:
--
-- "Represents a dimension box according to the CSS box model."
data StyleBorderWidth = StyleBorderWidth
  {
    styleBorderWidthTop    :: Int
  , styleBorderWidthRight  :: Int
  , styleBorderWidthBottom :: Int
  , styleBorderWidthLeft   :: Int
  } deriving (Show)




data StyleBorderColor = StyleBorderColor
  {
    styleBorderColorTop    :: Int -- TODO: replace the Int type with Color type
  , styleBorderColorRight  :: Int
  , styleBorderColorBottom :: Int
  , styleBorderColorLeft   :: Int
  } deriving (Show)




-- TODO: in dillo the margin variables were of type Box. The comment for Box
-- type was:
--
-- Represents a dimension box according to the CSS box model.
data StyleMargin = StyleMargin
  {
    styleMarginTop    :: Int
  , styleMarginRight  :: Int
  , styleMarginBottom :: Int
  , styleMarginLeft   :: Int
  } deriving (Show)




-- TODO: in dillo the padding variables were of type Box. The comment for Box
-- type was:
--
-- Represents a dimension box according to the CSS box model.
data StylePadding = StylePadding
  {
    stylePaddingTop    :: Int
  , stylePaddingRight  :: Int
  , stylePaddingBottom :: Int
  , stylePaddingLeft   :: Int
  } deriving (Show)




data StyleAttrs = StyleAttrs
  {
    styleFontAttrs         :: FontAttrs
  , styleBorderCollapse    :: Int
  , styleBorderStyle       :: StyleBorderStyle
  , styleBorderWidth       :: StyleBorderWidth
  , styleBorderColor       :: StyleBorderColor
  , styleMargin            :: StyleMargin
  , stylePadding           :: StylePadding
  , styleTextAlign         :: Int
  , styleTextDecoration    :: Int
  , styleTextIndent        :: DwLength
  , styleTextTransform     :: Int
  , styleVerticalAlign     :: Int
  , styleWhiteSpace        :: Int
  , styleWidth             :: DwLength
  , styleHeight            :: DwLength
  , styleLineHeight        :: DwLength
  , styleListStylePosition :: Int
  , styleListStyleType     :: Int
  , styleDisplay           :: Int
  , styleColor             :: Int -- TODO: change the type to Color
  , styleBackgroundColor   :: Int -- TODO: change the type to Color
  , styleCursor            :: Int
  , styleHBorderSpacing    :: Int
  , styleVBorderSpacing    :: Int
  , styleWordSpacing       :: Int

  , styleXLink             :: Int
  --Either x_lang[0] == x_lang[1] == 0 (no language set), or x_lang contains
  --the RFC 1766 country code in lower case letters. (Only two letters
  --allowed, currently.)
  , styleXLang             :: T.Text
  , styleXImg              :: Int
  , styleXTooltip          :: T.Text

  } deriving (Show)

