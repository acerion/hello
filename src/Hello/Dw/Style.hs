{-
Copyright (C) 2021-2023 Kamil Ignacak acerion@wp.pl

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




{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}




module Hello.Dw.Style
  (
    StyleAttrs (..)
  , StyleBorderStyle (..)
  , StyleBorderWidth (..)
  , StyleBorderColor (..)
  , StyleMargin (..)
  , StylePadding (..)

  , defaultStyleAttrs

  , styleAttrsInitValues
  , styleAttrsEqual
  , styleAttrsHashValue
  , styleAttrsCopy
  , styleAttrsReset
  )
where




import Data.Text as T
--import Debug.Trace
import Data.Word

import Hello.Dw.DwLength
import Hello.Dw.FontAttrs




data StyleBorderStyle = StyleBorderStyle
  {
    styleBorderStyleTop    :: Int
  , styleBorderStyleRight  :: Int
  , styleBorderStyleBottom :: Int
  , styleBorderStyleLeft   :: Int
  } deriving (Show)

defaultStyleBorderStyle :: StyleBorderStyle
defaultStyleBorderStyle = StyleBorderStyle
  {
    styleBorderStyleTop    = 0
  , styleBorderStyleRight  = 0
  , styleBorderStyleBottom = 0
  , styleBorderStyleLeft   = 0
  }




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

defaultStyleBorderWidth :: StyleBorderWidth
defaultStyleBorderWidth = StyleBorderWidth
  {
    styleBorderWidthTop    = 0
  , styleBorderWidthRight  = 0
  , styleBorderWidthBottom = 0
  , styleBorderWidthLeft   = 0
  }




data StyleBorderColor = StyleBorderColor
  {
    styleBorderColorTop    :: Int -- TODO: replace the Int type with Color type
  , styleBorderColorRight  :: Int
  , styleBorderColorBottom :: Int
  , styleBorderColorLeft   :: Int
  } deriving (Show)

defaultStyleBorderColor :: StyleBorderColor
defaultStyleBorderColor = StyleBorderColor
  {
    styleBorderColorTop    = 0
  , styleBorderColorRight  = 0
  , styleBorderColorBottom = 0
  , styleBorderColorLeft   = 0
  }




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

defaultStyleMargin :: StyleMargin
defaultStyleMargin = StyleMargin
  {
    styleMarginTop    = 0
  , styleMarginRight  = 0
  , styleMarginBottom = 0
  , styleMarginLeft   = 0
  }




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

defaultStylePadding :: StylePadding
defaultStylePadding = StylePadding
  {
    stylePaddingTop    = 0
  , stylePaddingRight  = 0
  , stylePaddingBottom = 0
  , stylePaddingLeft   = 0
  }




data StyleAttrs = StyleAttrs
  {
    styleAttrsRef          :: Int
  , styleFontAttrs         :: FontAttrs
  , styleBorderCollapse    :: Int
  , styleBorderStyle       :: StyleBorderStyle
  , styleBorderWidth       :: StyleBorderWidth
  , styleBorderColor       :: StyleBorderColor
  , styleMargin            :: StyleMargin
  , stylePadding           :: StylePadding
  , styleTextAlign         :: Int     -- TODO: use TextAlignType type
  , styleTextDecoration    :: Word32  -- TODO: use proper type?
  , styleTextIndent        :: DwLength
  , styleTextTransform     :: Int -- TODO: use TextTransform type
  , styleVerticalAlign     :: Int -- TODO: use VAlignType
  , styleWhiteSpace        :: Int -- TODO: use WhiteSpace type
  , styleWidth             :: DwLength
  , styleHeight            :: DwLength
  , styleLineHeight        :: DwLength
  , styleListStylePosition :: Int
  , styleListStyleType     :: Int
  , styleDisplay           :: Int
  , styleColor             :: Int -- TODO: change the type to Color
  , styleBackgroundColor   :: Int -- TODO: change the type to Color
  , styleCursor            :: Int -- TODO: use Cursor type
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




defaultStyleAttrs :: StyleAttrs
defaultStyleAttrs = StyleAttrs
  {
    styleAttrsRef          = 0
  , styleFontAttrs         = defaultFontAttrs
  , styleBorderCollapse    = 0
  , styleBorderStyle       = defaultStyleBorderStyle
  , styleBorderWidth       = defaultStyleBorderWidth
  , styleBorderColor       = defaultStyleBorderColor
  , styleMargin            = defaultStyleMargin
  , stylePadding           = defaultStylePadding
  , styleTextAlign         = 0
  , styleTextDecoration    = 0
  , styleTextIndent        = createAutoDwLength
  , styleTextTransform     = 0
  , styleVerticalAlign     = 0
  , styleWhiteSpace        = 0
  , styleWidth             = createAutoDwLength
  , styleHeight            = createAutoDwLength
  , styleLineHeight        = createAutoDwLength
  , styleListStylePosition = 0
  , styleListStyleType     = 0
  , styleDisplay           = 0
  , styleColor             = 0
  , styleBackgroundColor   = 0
  , styleCursor            = 0
  , styleHBorderSpacing    = 0
  , styleVBorderSpacing    = 0
  , styleWordSpacing       = 0

  , styleXLink             = 0
  , styleXLang             = ""
  , styleXImg              = 0
  , styleXTooltip          = ""
  }




styleAttrsInitValues :: StyleAttrs -> StyleAttrs
styleAttrsInitValues sa = sa { styleTextAlign      = 0  -- TEXT_ALIGN_LEFT == 0
                             , styleTextDecoration = 0  -- TEXT_DECORATION_NONE == 0
                             , styleTextTransform  = 0  -- TEXT_TRANSFORM_NONE == 0
                             , styleCursor         = 1  -- CURSOR_DEFAULT == 1
                             , styleWhiteSpace     = 0  -- WHITE_SPACE_NORMAL = 0
                             }




styleAttrsEqual :: StyleAttrs -> StyleAttrs -> Bool
styleAttrsEqual sa1 sa2 = and $ fmap (\ field -> field sa1 == field sa2)
                          [ styleTextAlign
                          , fromIntegral . styleTextDecoration
                          , styleTextTransform
                          , styleCursor
                          , styleWhiteSpace
                          ]




styleAttrsHashValue :: StyleAttrs -> Int
styleAttrsHashValue sa = styleTextAlign sa
                         + (fromIntegral . styleTextDecoration $ sa)
                         + styleTextTransform sa
                         + styleCursor sa
                         + styleWhiteSpace sa




styleAttrsCopy :: StyleAttrs -> StyleAttrs -> StyleAttrs
styleAttrsCopy to from = to { styleTextAlign      = styleTextAlign from
                            , styleTextDecoration = styleTextDecoration from
                            , styleTextTransform  = styleTextTransform from
                            , styleCursor         = styleCursor from
                            , styleWhiteSpace     = styleWhiteSpace from
                            }




-- This function doesn't do anything yet. It's not called anywhere.
styleAttrsReset :: StyleAttrs -> StyleAttrs
styleAttrsReset attrs = attrs

