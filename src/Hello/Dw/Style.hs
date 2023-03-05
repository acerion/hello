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

  , styleAttrsSetCollapseTableAttrs
  , styleAttrsSetBorderStyle
  )
where




import Data.Text as T
--import Debug.Trace
import Data.Word

import Hello.Dw.DwLength
import Hello.Dw.FontAttrs




data StyleBorderStyle = StyleBorderStyle
  {
    styleBorderStyleTop    :: Int   -- TODO: use BorderStyle type instead of int
  , styleBorderStyleRight  :: Int
  , styleBorderStyleBottom :: Int
  , styleBorderStyleLeft   :: Int
  } deriving (Eq, Show)

defaultStyleBorderStyle :: StyleBorderStyle
defaultStyleBorderStyle = StyleBorderStyle
  {
    styleBorderStyleTop    = 0 -- BORDER_NONE == 0
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
  , styleBorderCollapse    :: Int -- TODO: use BorderCollapse type
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
  , styleListStylePosition :: Int  -- TODO: use ListStylePosition
  , styleListStyleType     :: Int  -- TODO: use ListStyleType
  , styleDisplay           :: Int
  , styleColor             :: Int -- TODO: change the type to Color
  , styleBackgroundColor   :: Int -- TODO: change the type to Color
  , styleCursor            :: Int -- TODO: use Cursor type
  , styleHBorderSpacing    :: Int
  , styleVBorderSpacing    :: Int
  , styleWordSpacing       :: Int

  , styleBgPositionX       :: DwLength -- "left" defined by "0%" etc. (see CSS spec)
  , styleBgPositionY       :: DwLength -- "top" defined by "0%" etc. (see CSS spec)

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
  , styleBgPositionX       = createPercentageDwLength 0
  , styleBgPositionY       = createPercentageDwLength 0

  , styleXLink             = -1
  , styleXLang             = ""
  , styleXImg              = -1
  , styleXTooltip          = ""
  }




-- TODO: doesn't this duplicate defaultStyleAttrs?
styleAttrsInitValues :: StyleAttrs -> StyleAttrs
styleAttrsInitValues sa = sa { styleTextAlign      = 0  -- TEXT_ALIGN_LEFT == 0
                             , styleTextDecoration = 0  -- TEXT_DECORATION_NONE == 0
                             , styleTextTransform  = 0  -- TEXT_TRANSFORM_NONE == 0
                             , styleCursor         = 1  -- CURSOR_DEFAULT == 1
                             , styleWhiteSpace     = 0  -- WHITE_SPACE_NORMAL = 0
                             , styleListStylePosition = 1 -- LIST_STYLE_POSITION_OUTSIDE == 1
                             , styleListStyleType  = 0    -- LIST_STYLE_TYPE_DISC == 0
                             , styleBorderCollapse = 0    -- BORDER_MODEL_SEPARATE == 0;
                             , styleBorderStyle    = defaultStyleBorderStyle
                             , styleWidth          = createAutoDwLength
                             , styleHeight         = createAutoDwLength
                             , styleTextIndent     = createAutoDwLength
                             , styleLineHeight     = createAutoDwLength
                             , styleVerticalAlign  = 3 -- VALIGN_BASELINE == 3
                             , styleBgPositionX    = createPercentageDwLength 0
                             , styleBgPositionY    = createPercentageDwLength 0
                             }




-- TODO: This all can be simplified through Eq class.
styleAttrsEqual :: StyleAttrs -> StyleAttrs -> Bool
styleAttrsEqual sa1 sa2 = and
                          [ styleTextAlign sa1 == styleTextAlign sa2
                          , styleTextDecoration sa1 == styleTextDecoration sa2
                          , styleTextTransform sa1 == styleTextTransform sa2
                          , styleCursor sa1 == styleCursor sa2
                          , styleWhiteSpace sa1 == styleWhiteSpace sa2
                          , styleListStylePosition sa1 == styleListStylePosition sa2
                          , styleListStyleType sa1 == styleListStyleType sa2
                          , styleXLink sa1 == styleXLink sa2
                          , styleXImg sa1 == styleXImg sa2
                          , styleBorderCollapse sa1 == styleBorderCollapse sa2
                          , styleBorderStyle sa1 == styleBorderStyle sa2
                          , styleWidth sa1 == styleWidth sa2
                          , styleHeight sa1 == styleHeight sa2
                          , styleTextIndent sa1 == styleTextIndent sa2
                          , styleLineHeight sa1 == styleLineHeight sa2
                          , styleVerticalAlign sa1 == styleVerticalAlign sa2
                          , styleBgPositionX sa1 == styleBgPositionX sa2
                          , styleBgPositionY sa1 == styleBgPositionY sa2
                          ]




styleAttrsHashValue :: StyleAttrs -> Int
styleAttrsHashValue sa = styleTextAlign sa
                         + (fromIntegral . styleTextDecoration $ sa)
                         + styleTextTransform sa
                         + styleCursor sa
                         + styleWhiteSpace sa
                         + styleListStylePosition sa
                         + styleListStyleType sa
                         + styleXLink sa
                         + styleXImg sa
                         + styleBorderCollapse sa
                         + (styleBorderStyleTop . styleBorderStyle $ sa)
                         + (styleBorderStyleRight . styleBorderStyle $ sa)
                         + (styleBorderStyleBottom . styleBorderStyle $ sa)
                         + (styleBorderStyleLeft . styleBorderStyle $ sa)
                         -- + styleWidth sa    -- TODO: re-enable
                         -- + styleHeight sa   -- TODO: re-enable
                         -- + styleTextIndent sa -- TODO: re-enable
                         -- + styleLineHeight sa -- TODO: re-enable
                         + styleVerticalAlign sa
                         -- + styleBgPositionX sa -- TODO: re-enable
                         -- + styleBgPositionY sa -- TODO: re-enable





-- TODO: this can be replaced by simple assignment
styleAttrsCopy :: StyleAttrs -> StyleAttrs -> StyleAttrs
styleAttrsCopy to from = to { styleTextAlign      = styleTextAlign from
                            , styleTextDecoration = styleTextDecoration from
                            , styleTextTransform  = styleTextTransform from
                            , styleCursor         = styleCursor from
                            , styleWhiteSpace     = styleWhiteSpace from
                            , styleListStylePosition = styleListStylePosition from
                            , styleListStyleType  = styleListStyleType from
                            , styleXLink          = styleXLink from
                            , styleXImg           = styleXImg from
                            , styleBorderCollapse = styleBorderCollapse from
                            , styleBorderStyle    = styleBorderStyle from
                            , styleWidth          = styleWidth from
                            , styleHeight         = styleHeight from
                            , styleTextIndent     = styleTextIndent from
                            , styleLineHeight     = styleLineHeight from
                            , styleVerticalAlign  = styleVerticalAlign from
                            , styleBgPositionX    = styleBgPositionX from
                            , styleBgPositionY    = styleBgPositionY from
                            }




styleAttrsReset :: StyleAttrs -> StyleAttrs
styleAttrsReset attrs = attrs
  { styleXImg           = -1
  , styleBorderStyle    = defaultStyleBorderStyle
  , styleWidth          = createAutoDwLength
  , styleHeight         = createAutoDwLength
  , styleVerticalAlign  = 3 -- VALIGN_BASELINE == 3
  , styleBgPositionX    = createPercentageDwLength 0
  , styleBgPositionY    = createPercentageDwLength 0
  }




styleAttrsSetCollapseTableAttrs :: StyleAttrs -> StyleAttrs -> StyleAttrs
styleAttrsSetCollapseTableAttrs attrsTable attrsCell =
  attrsTable { styleBorderStyle = styleBorderStyle attrsCell }




styleAttrsSetBorderStyle :: StyleAttrs -> Int -> StyleAttrs
styleAttrsSetBorderStyle sa val = sa
  { styleBorderStyle = (styleBorderStyle sa) { styleBorderStyleTop    = val
                                             , styleBorderStyleRight  = val
                                             , styleBorderStyleBottom = val
                                             , styleBorderStyleLeft   = val
                                             }
  }

