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
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Dw.Style
  (
    FfiStyleBorderWidth (..)
  , peekStyleBorderWidth
  , pokeStyleBorderWidth

  , FfiStyleBorderColor (..)
  , peekStyleBorderColor
  , pokeStyleBorderColor

  , FfiStyleMargin (..)
  , peekStyleMargin
  , pokeStyleMargin

  , FfiStylePadding (..)
  , peekStylePadding
  , pokeStylePadding

{-
  , FfiStyleAttrs
  , peekStyleAttrs
  , pokeStyleAttrs
-}
  )
where




import Prelude
import Foreign
import Foreign.C.Types
--import Debug.Trace

import Hello.Dw.Style




#include "../../hello.h"




data FfiStyleBorderWidth = FfiStyleBorderWidth
  {
    cStyleBorderWidthTop    :: CInt
  , cStyleBorderWidthRight  :: CInt
  , cStyleBorderWidthBottom :: CInt
  , cStyleBorderWidthLeft   :: CInt
  } deriving (Show)





instance Storable FfiStyleBorderWidth where
  sizeOf    _ = #{size c_border_width_t}
  alignment _ = #{alignment c_border_width_t}

  poke ptr (FfiStyleBorderWidth t r b l) = do
    #{poke c_border_width_t, top}    ptr t
    #{poke c_border_width_t, right}  ptr r
    #{poke c_border_width_t, bottom} ptr b
    #{poke c_border_width_t, left}   ptr l


  peek ptr = do
    t <- #{peek c_border_width_t, top} ptr
    r <- #{peek c_border_width_t, right}  ptr
    b <- #{peek c_border_width_t, bottom} ptr
    l <- #{peek c_border_width_t, left} ptr
    return (FfiStyleBorderWidth t r b l)




peekStyleBorderWidth :: Ptr FfiStyleBorderWidth -> IO StyleBorderWidth
peekStyleBorderWidth ptrStructBorderWidth = do
  ffiWidth <- peek ptrStructBorderWidth
  return StyleBorderWidth
    {
      styleBorderWidthTop    = fromIntegral . cStyleBorderWidthTop    $ ffiWidth
    , styleBorderWidthRight  = fromIntegral . cStyleBorderWidthRight  $ ffiWidth
    , styleBorderWidthBottom = fromIntegral . cStyleBorderWidthBottom $ ffiWidth
    , styleBorderWidthLeft   = fromIntegral . cStyleBorderWidthLeft   $ ffiWidth
    }




pokeStyleBorderWidth :: StyleBorderWidth -> Ptr FfiStyleBorderWidth -> IO ()
pokeStyleBorderWidth style ptrStructBorderWidth = do
  let top    = fromIntegral . styleBorderWidthTop    $ style
  let right  = fromIntegral . styleBorderWidthRight  $ style
  let bottom = fromIntegral . styleBorderWidthBottom $ style
  let left   = fromIntegral . styleBorderWidthLeft   $ style

  poke ptrStructBorderWidth $ FfiStyleBorderWidth top right bottom left




----------------------------------------




data FfiStyleBorderColor = FfiStyleBorderColor
  {
    cStyleBorderColorTop    :: CInt
  , cStyleBorderColorRight  :: CInt
  , cStyleBorderColorBottom :: CInt
  , cStyleBorderColorLeft   :: CInt
  } deriving (Show)




instance Storable FfiStyleBorderColor where
  sizeOf    _ = #{size c_border_color_t}
  alignment _ = #{alignment c_border_color_t}

  poke ptr (FfiStyleBorderColor t r b l) = do
    #{poke c_border_color_t, top}    ptr t
    #{poke c_border_color_t, right}  ptr r
    #{poke c_border_color_t, bottom} ptr b
    #{poke c_border_color_t, left}   ptr l


  peek ptr = do
    t <- #{peek c_border_color_t, top} ptr
    r <- #{peek c_border_color_t, right}  ptr
    b <- #{peek c_border_color_t, bottom} ptr
    l <- #{peek c_border_color_t, left} ptr
    return (FfiStyleBorderColor t r b l)




peekStyleBorderColor :: Ptr FfiStyleBorderColor -> IO StyleBorderColor
peekStyleBorderColor ptrStructBorderColor = do
  ffiStyle <- peek ptrStructBorderColor
  return StyleBorderColor
    {
      styleBorderColorTop    = fromIntegral . cStyleBorderColorTop    $ ffiStyle
    , styleBorderColorRight  = fromIntegral . cStyleBorderColorRight  $ ffiStyle
    , styleBorderColorBottom = fromIntegral . cStyleBorderColorBottom $ ffiStyle
    , styleBorderColorLeft   = fromIntegral . cStyleBorderColorLeft   $ ffiStyle
    }




pokeStyleBorderColor :: StyleBorderColor -> Ptr FfiStyleBorderColor -> IO ()
pokeStyleBorderColor color ptrStructBorderColor = do
  let top    = fromIntegral . styleBorderColorTop    $ color
  let right  = fromIntegral . styleBorderColorRight  $ color
  let bottom = fromIntegral . styleBorderColorBottom $ color
  let left   = fromIntegral . styleBorderColorLeft   $ color

  poke ptrStructBorderColor $ FfiStyleBorderColor top right bottom left




----------------------------------------




data FfiStyleMargin = FfiStyleMargin
  {
    cStyleMarginTop    :: CInt
  , cStyleMarginRight  :: CInt
  , cStyleMarginBottom :: CInt
  , cStyleMarginLeft   :: CInt
  } deriving (Show)




instance Storable FfiStyleMargin where
  sizeOf    _ = #{size c_style_margin_t}
  alignment _ = #{alignment c_style_margin_t}

  poke ptr (FfiStyleMargin t r b l) = do
    #{poke c_style_margin_t, top}    ptr t
    #{poke c_style_margin_t, right}  ptr r
    #{poke c_style_margin_t, bottom} ptr b
    #{poke c_style_margin_t, left}   ptr l


  peek ptr = do
    t <- #{peek c_style_margin_t, top} ptr
    r <- #{peek c_style_margin_t, right}  ptr
    b <- #{peek c_style_margin_t, bottom} ptr
    l <- #{peek c_style_margin_t, left} ptr
    return (FfiStyleMargin t r b l)




peekStyleMargin :: Ptr FfiStyleMargin -> IO StyleMargin
peekStyleMargin ptrStructStyleMarginArg = do
  ffiMargin <- peek ptrStructStyleMarginArg
  return StyleMargin
    {
      styleMarginTop    = fromIntegral . cStyleMarginTop    $ ffiMargin
    , styleMarginRight  = fromIntegral . cStyleMarginRight  $ ffiMargin
    , styleMarginBottom = fromIntegral . cStyleMarginBottom $ ffiMargin
    , styleMarginLeft   = fromIntegral . cStyleMarginLeft   $ ffiMargin
    }




pokeStyleMargin :: StyleMargin -> Ptr FfiStyleMargin -> IO ()
pokeStyleMargin style ptrStructStyleMarginArg = do
  let top    = fromIntegral . styleMarginTop    $ style
  let right  = fromIntegral . styleMarginRight  $ style
  let bottom = fromIntegral . styleMarginBottom $ style
  let left   = fromIntegral . styleMarginLeft   $ style

  poke ptrStructStyleMarginArg $ FfiStyleMargin top right bottom left




----------------------------------------




data FfiStylePadding = FfiStylePadding
  {
    cStylePaddingTop    :: CInt
  , cStylePaddingRight  :: CInt
  , cStylePaddingBottom :: CInt
  , cStylePaddingLeft   :: CInt
  } deriving (Show)




instance Storable FfiStylePadding where
  sizeOf    _ = #{size c_style_padding_t}
  alignment _ = #{alignment c_style_padding_t}

  poke ptr (FfiStylePadding t r b l) = do
    #{poke c_style_padding_t, top}    ptr t
    #{poke c_style_padding_t, right}  ptr r
    #{poke c_style_padding_t, bottom} ptr b
    #{poke c_style_padding_t, left}   ptr l


  peek ptr = do
    t <- #{peek c_style_padding_t, top} ptr
    r <- #{peek c_style_padding_t, right}  ptr
    b <- #{peek c_style_padding_t, bottom} ptr
    l <- #{peek c_style_padding_t, left} ptr
    return (FfiStylePadding t r b l)




peekStylePadding :: Ptr FfiStylePadding -> IO StylePadding
peekStylePadding ptrStructStylePaddingArg = do
  ffiPadding <- peek ptrStructStylePaddingArg
  return StylePadding
    {
      stylePaddingTop    = fromIntegral . cStylePaddingTop    $ ffiPadding
    , stylePaddingRight  = fromIntegral . cStylePaddingRight  $ ffiPadding
    , stylePaddingBottom = fromIntegral . cStylePaddingBottom $ ffiPadding
    , stylePaddingLeft   = fromIntegral . cStylePaddingLeft   $ ffiPadding
    }




pokeStylePadding :: StylePadding -> Ptr FfiStylePadding -> IO ()
pokeStylePadding style ptrStructStylePaddingArg = do
  let top    = fromIntegral . stylePaddingTop    $ style
  let right  = fromIntegral . stylePaddingRight  $ style
  let bottom = fromIntegral . stylePaddingBottom $ style
  let left   = fromIntegral . stylePaddingLeft   $ style

  poke ptrStructStylePaddingArg $ FfiStylePadding top right bottom left




----------------------------------------



{-
data FfiStyleAttrs = FfiStyleAttrs
  {
    iStyleAttrsRef             :: CInt
  } deriving (Show)




instance Storable FfiStyleAttrs where
  sizeOf    _ = #{size c_style_attrs_t}
  alignment _ = #{alignment c_style_attrs_t}

  peek ptr = do
    ref              <- #{peek c_style_attrs_t, c_style_attrs_ref}   ptr
    return (FfiStyleAttrs ref)




  poke ptr (FfiStyleAttrs cStyleAttrsRef) = do
    #{poke c_style_attrs_t, c_style_attrs_ref} ptr cStyleAttrsRef




peekStyleAttrs :: Ptr FfiStyleAttrs -> IO StyleAttrs
peekStyleAttrs ptrStructStyleAttrs = do
  ffiAttrs  <- peek ptrStructStyleAttrs
  gAttrs    <- globalStyleAttrsGet . fromIntegral . iStyleAttrsRef $ ffiAttrs

  let attrs = defaultStyleAttrs {
      styleAttrsRef    = styleAttrsRef gAttrs
    , styleFontAttrs   = styleFontAttrs gAttrs
    , styleBorderCollapse = styleBorderCollapse gAttrs
    , styleBorderStyle = styleBorderStyle gAttrs
    , styleBorderWidth = styleBorderWidth gAttrs
    , styleBorderColor = styleBorderColor gAttrs
    , styleMargin      = styleMargin gAttrs
    , stylePadding     = stylePadding gAttrs

    , styleTextAlign      = styleTextAlign gAttrs
    , styleTextDecoration = styleTextDecoration gAttrs
    , styleTextIndent     = styleTextIndent gAttrs
    , styleTextTransform  = styleTextTransform gAttrs

    , styleVerticalAlign  = styleVerticalAlign gAttrs
    , styleWhiteSpace     = styleWhiteSpace gAttrs

    , styleWidth          = styleWidth gAttrs
    , styleHeight         = styleHeight gAttrs
    , styleLineHeight     = styleLineHeight gAttrs

    , styleBgPositionX = styleBgPositionX gAttrs
    , styleBgPositionY = styleBgPositionY gAttrs

    , styleListStylePosition      = styleListStylePosition gAttrs
    , styleListStyleType          = styleListStyleType gAttrs

    , styleDisplay                = styleDisplay gAttrs
    , styleColor                  = styleColor gAttrs
    , styleBackgroundColor        = styleBackgroundColor gAttrs
    , styleCursor                 = styleCursor gAttrs
    , styleHorizBorderSpacing     = styleHorizBorderSpacing gAttrs
    , styleVertBorderSpacing      = styleVertBorderSpacing gAttrs
    , styleWordSpacing            = styleWordSpacing gAttrs
    , styleXLink                  = styleXLink gAttrs
    , styleXLang                  = styleXLang gAttrs
    , styleXImg                   = styleXImg gAttrs
    , styleXTooltip               = styleXTooltip gAttrs
    }

  return attrs




pokeStyleAttrs :: StyleAttrs -> Ptr FfiStyleAttrs -> IO ()
pokeStyleAttrs attrs ptrStructStyleAttrs = do

  -- I'm peeking a pointer in 'pokeStyleAttrs' function to get access to
  -- pointer-members of ptrStructStyleAttrs. When I will have access to the
  -- pointer-members, I will be able to poke them with values passed through
  -- 'attrs'.
  let cStyleAttrsRef :: CInt = fromIntegral . styleAttrsRef $ attrs
  poke ptrStructStyleAttrs $ FfiStyleAttrs cStyleAttrsRef

-}


