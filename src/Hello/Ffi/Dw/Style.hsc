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
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Dw.Style
  (
    FfiStyleBorderStyle (..)
  , peekStyleBorderStyle
  , pokeStyleBorderStyle

  , FfiStyleBorderWidth (..)
  , peekStyleBorderWidth
  , pokeStyleBorderWidth

  , FfiStyleMargin (..)
  , peekStyleMargin
  , pokeStyleMargin

  , FfiStylePadding (..)
  , peekStylePadding
  , pokeStylePadding

  , FfiStyleAttrs
  , peekStyleAttrs
  , pokeStyleAttrs
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E

import Hello.Dw.DwLength
import Hello.Dw.Style

import Hello.Ffi.Dw.DwLength
import Hello.Ffi.Utils




#include "../../hello.h"




data FfiStyleBorderStyle = FfiStyleBorderStyle
  {
    cStyleBorderStyleTop    :: CInt
  , cStyleBorderStyleRight  :: CInt
  , cStyleBorderStyleBottom :: CInt
  , cStyleBorderStyleLeft   :: CInt
  } deriving (Show)




instance Storable FfiStyleBorderStyle where
  sizeOf    _ = #{size c_border_style_t}
  alignment _ = #{alignment c_border_style_t}

  poke ptr (FfiStyleBorderStyle t r b l) = do
    #{poke c_border_style_t, top}    ptr t
    #{poke c_border_style_t, right}  ptr r
    #{poke c_border_style_t, bottom} ptr b
    #{poke c_border_style_t, left}   ptr l


  peek ptr = do
    t <- #{peek c_border_style_t, top} ptr
    r <- #{peek c_border_style_t, right}  ptr
    b <- #{peek c_border_style_t, bottom} ptr
    l <- #{peek c_border_style_t, left} ptr
    return (FfiStyleBorderStyle t r b l)




peekStyleBorderStyle :: Ptr FfiStyleBorderStyle -> IO StyleBorderStyle
peekStyleBorderStyle ptrStructBorderStyle = do
  ffiStyle <- peek ptrStructBorderStyle
  return StyleBorderStyle
    {
      styleBorderStyleTop    = fromIntegral . cStyleBorderStyleTop    $ ffiStyle
    , styleBorderStyleRight  = fromIntegral . cStyleBorderStyleRight  $ ffiStyle
    , styleBorderStyleBottom = fromIntegral . cStyleBorderStyleBottom $ ffiStyle
    , styleBorderStyleLeft   = fromIntegral . cStyleBorderStyleLeft   $ ffiStyle
    }




pokeStyleBorderStyle :: StyleBorderStyle -> Ptr FfiStyleBorderStyle -> IO ()
pokeStyleBorderStyle style ptrStructBorderStyle = do
  let top    = fromIntegral . styleBorderStyleTop    $ style
  let right  = fromIntegral . styleBorderStyleRight  $ style
  let bottom = fromIntegral . styleBorderStyleBottom $ style
  let left   = fromIntegral . styleBorderStyleLeft   $ style

  poke ptrStructBorderStyle $ FfiStyleBorderStyle top right bottom left




----------------------------------------




data FfiStyleBorderWidth = FfiStyleBorderWidth
  {
    cStyleBorderWidthTop    :: CInt
  , cStyleBorderWidthRight  :: CInt
  , cStyleBorderWidthBottom :: CInt
  , cStyleBorderWidthLeft   :: CInt
  } deriving (Show)





instance Storable FfiStyleBorderWidth where
  sizeOf    _ = #{size c_border_style_t}
  alignment _ = #{alignment c_border_style_t}

  poke ptr (FfiStyleBorderWidth t r b l) = do
    #{poke c_border_style_t, top}    ptr t
    #{poke c_border_style_t, right}  ptr r
    #{poke c_border_style_t, bottom} ptr b
    #{poke c_border_style_t, left}   ptr l


  peek ptr = do
    t <- #{peek c_border_style_t, top} ptr
    r <- #{peek c_border_style_t, right}  ptr
    b <- #{peek c_border_style_t, bottom} ptr
    l <- #{peek c_border_style_t, left} ptr
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
peekStyleMargin ptrStructStyleMargin = do
  ffiMargin <- peek ptrStructStyleMargin
  return StyleMargin
    {
      styleMarginTop    = fromIntegral . cStyleMarginTop    $ ffiMargin
    , styleMarginRight  = fromIntegral . cStyleMarginRight  $ ffiMargin
    , styleMarginBottom = fromIntegral . cStyleMarginBottom $ ffiMargin
    , styleMarginLeft   = fromIntegral . cStyleMarginLeft   $ ffiMargin
    }




pokeStyleMargin :: StyleMargin -> Ptr FfiStyleMargin -> IO ()
pokeStyleMargin style ptrStructStyleMargin = do
  let top    = fromIntegral . styleMarginTop    $ style
  let right  = fromIntegral . styleMarginRight  $ style
  let bottom = fromIntegral . styleMarginBottom $ style
  let left   = fromIntegral . styleMarginLeft   $ style

  poke ptrStructStyleMargin $ FfiStyleMargin top right bottom left




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
peekStylePadding ptrStructStylePadding = do
  ffiPadding <- peek ptrStructStylePadding
  return StylePadding
    {
      stylePaddingTop    = fromIntegral . cStylePaddingTop    $ ffiPadding
    , stylePaddingRight  = fromIntegral . cStylePaddingRight  $ ffiPadding
    , stylePaddingBottom = fromIntegral . cStylePaddingBottom $ ffiPadding
    , stylePaddingLeft   = fromIntegral . cStylePaddingLeft   $ ffiPadding
    }




pokeStylePadding :: StylePadding -> Ptr FfiStylePadding -> IO ()
pokeStylePadding style ptrStructStylePadding = do
  let top    = fromIntegral . stylePaddingTop    $ style
  let right  = fromIntegral . stylePaddingRight  $ style
  let bottom = fromIntegral . stylePaddingBottom $ style
  let left   = fromIntegral . stylePaddingLeft   $ style

  poke ptrStructStylePadding $ FfiStylePadding top right bottom left




----------------------------------------




data FfiStyleAttrs = FfiStyleAttrs
  {
    ptrStructStyleBorderStyle  :: Ptr FfiStyleBorderStyle
  , ptrStructStyleBorderWidth  :: Ptr FfiStyleBorderWidth
  , ptrStructStyleBorderColor  :: Ptr FfiStyleBorderColor
  , ptrStructStyleMargin       :: Ptr FfiStyleMargin
  , ptrStructStylePadding      :: Ptr FfiStylePadding
  , iStyleTextAlign            :: CInt
  , iTextDecoration            :: CInt
  , ptrStructStyleTextIndent   :: Ptr FfiDwLength
  , iTextTransform             :: CInt
  , iVerticalAlign             :: CInt
  , iWhiteSpace                :: CInt
  , ptrStructWidth             :: Ptr FfiDwLength
  , ptrStructHeight            :: Ptr FfiDwLength
  , ptrStructLineHeight        :: Ptr FfiDwLength
  , iListStylePosition         :: CInt
  , iListStyleType             :: CInt
  , iDisplay                   :: CInt
  , iColor                     :: CInt
  , iCursor                    :: CInt
  , iHBorderSpacing            :: CInt
  , iVBorderSpacing            :: CInt
  } deriving (Show)




instance Storable FfiStyleAttrs where
  sizeOf    _ = #{size c_style_attrs_t}
  alignment _ = #{alignment c_style_attrs_t}

  peek ptr = do
    borderStyle      <- #{peek c_style_attrs_t, c_border_style}      ptr
    borderWidth      <- #{peek c_style_attrs_t, c_border_width}      ptr
    borderColor      <- #{peek c_style_attrs_t, c_border_color}      ptr
    margin           <- #{peek c_style_attrs_t, c_margin}            ptr
    padding          <- #{peek c_style_attrs_t, c_padding}           ptr
    textAlign        <- #{peek c_style_attrs_t, c_text_align}        ptr
    textDecoration   <- #{peek c_style_attrs_t, c_text_decoration}   ptr
    textIndent       <- #{peek c_style_attrs_t, c_text_indent}       ptr
    textTransform    <- #{peek c_style_attrs_t, c_text_transform}    ptr
    verticalAlign    <- #{peek c_style_attrs_t, c_vertical_align}    ptr
    whiteSpace       <- #{peek c_style_attrs_t, c_white_space}       ptr
    width            <- #{peek c_style_attrs_t, c_width}             ptr
    height           <- #{peek c_style_attrs_t, c_height}            ptr
    lineHeight       <- #{peek c_style_attrs_t, c_line_height}       ptr
    listStylePosition  <- #{peek c_style_attrs_t, c_list_style_position}  ptr
    listStyleType      <- #{peek c_style_attrs_t, c_list_style_type}      ptr
    display            <- #{peek c_style_attrs_t, c_display}              ptr
    color              <- #{peek c_style_attrs_t, c_color}                ptr
    cursor             <- #{peek c_style_attrs_t, c_cursor}               ptr
    hBorderSpacing     <- #{peek c_style_attrs_t, c_h_border_spacing}     ptr
    vBorderSpacing     <- #{peek c_style_attrs_t, c_v_border_spacing}     ptr
    return (FfiStyleAttrs borderStyle borderWidth borderColor margin padding textAlign textDecoration textIndent textTransform verticalAlign whiteSpace width height lineHeight listStylePosition listStyleType display color cursor hBorderSpacing vBorderSpacing)




  poke ptr (FfiStyleAttrs cBorderStyle cBorderWidth cBorderColor cMargin cPadding cTextAlign cTextDecoration cTextIndent cTextTransform cVerticalAlign cWhiteSpace cWidth cHeight cLineHeight cListStylePosition cListStyleType cDisplay cColor cCursor cHBorderSpacing cVBorderSpacing) = do
    #{poke c_style_attrs_t, c_border_style}    ptr cBorderStyle
    #{poke c_style_attrs_t, c_border_width}    ptr cBorderWidth
    #{poke c_style_attrs_t, c_border_color}    ptr cBorderColor
    #{poke c_style_attrs_t, c_margin}          ptr cMargin
    #{poke c_style_attrs_t, c_padding}         ptr cPadding
    #{poke c_style_attrs_t, c_text_align}      ptr cTextAlign
    #{poke c_style_attrs_t, c_text_decoration} ptr cTextDecoration
    #{poke c_style_attrs_t, c_text_indent}     ptr cTextIndent
    #{poke c_style_attrs_t, c_text_transform}  ptr cTextTransform
    #{poke c_style_attrs_t, c_vertical_align}  ptr cVerticalAlign
    #{poke c_style_attrs_t, c_white_space}     ptr cWhiteSpace
    #{poke c_style_attrs_t, c_width}           ptr cWidth
    #{poke c_style_attrs_t, c_height}          ptr cHeight
    #{poke c_style_attrs_t, c_line_height}          ptr cLineHeight
    #{poke c_style_attrs_t, c_list_style_position}  ptr cListStylePosition
    #{poke c_style_attrs_t, c_list_style_type}      ptr cListStyleType
    #{poke c_style_attrs_t, c_display}              ptr cDisplay
    #{poke c_style_attrs_t, c_color}                ptr cColor
    #{poke c_style_attrs_t, c_cursor}               ptr cCursor
    #{poke c_style_attrs_t, c_h_border_spacing}     ptr cHBorderSpacing
    #{poke c_style_attrs_t, c_v_border_spacing}     ptr cVBorderSpacing




peekStyleAttrs :: Ptr FfiStyleAttrs -> IO StyleAttrs
peekStyleAttrs ptrStructStyleAttrs = do
  ffiAttrs <- peek ptrStructStyleAttrs

  borderStyle <- peekStyleBorderStyle . ptrStructStyleBorderStyle $ ffiAttrs
  borderWidth <- peekStyleBorderWidth . ptrStructStyleBorderWidth $ ffiAttrs
  borderColor <- peekStyleBorderColor . ptrStructStyleBorderColor $ ffiAttrs
  margin      <- peekStyleMargin . ptrStructStyleMargin $ ffiAttrs
  padding     <- peekStylePadding . ptrStructStylePadding $ ffiAttrs
  tIndent     <- peekDwLength . ptrStructStyleTextIndent $ ffiAttrs

  width  <- peekDwLength . ptrStructWidth $ ffiAttrs
  height <- peekDwLength . ptrStructHeight $ ffiAttrs
  lineHeight <- peekDwLength . ptrStructLineHeight $ ffiAttrs

  return StyleAttrs
    {
      styleBorderStyle = borderStyle
    , styleBorderWidth = borderWidth
    , styleBorderColor = borderColor
    , styleMargin      = margin
    , stylePadding     = padding

    , styleTextAlign      = fromIntegral . iStyleTextAlign $ ffiAttrs
    , styleTextDecoration = fromIntegral . iTextDecoration $ ffiAttrs
    , styleTextIndent     = tIndent
    , styleTextTransform  = fromIntegral . iTextTransform $ ffiAttrs

    , styleVerticalAlign  = fromIntegral . iVerticalAlign $ ffiAttrs
    , styleWhiteSpace     = fromIntegral . iWhiteSpace $ ffiAttrs

    , styleWidth          = width
    , styleHeight         = height
    , styleLineHeight     = lineHeight

    , styleListStylePosition      = fromIntegral . iListStylePosition $ ffiAttrs
    , styleListStyleType          = fromIntegral . iListStyleType $ ffiAttrs

    , styleDisplay                = fromIntegral . iDisplay $ ffiAttrs
    , styleColor                  = fromIntegral . iColor $ ffiAttrs
    , styleCursor                 = fromIntegral . iCursor $ ffiAttrs
    , styleHBorderSpacing         = fromIntegral . iHBorderSpacing $ ffiAttrs
    , styleVBorderSpacing         = fromIntegral . iVBorderSpacing $ ffiAttrs
    }




pokeStyleAttrs :: StyleAttrs -> Ptr FfiStyleAttrs -> IO ()
pokeStyleAttrs attrs ptrStructStyleAttrs = do

  -- I'm peeking a pointer in 'pokeStyleAttrs' function to get access to
  -- pointer-members of ptrStructStyleAttrs. When I will have access to the
  -- pointer-members, I will be able to poke them with values passed through
  -- 'attrs'.
  ffiStyleAttrs :: FfiStyleAttrs <- peek ptrStructStyleAttrs

  -- getAccess to member-pointers, and then poke them
  let pBorderStyle :: Ptr FfiStyleBorderStyle = ptrStructStyleBorderStyle ffiStyleAttrs
  pokeStyleBorderStyle (styleBorderStyle attrs) pBorderStyle

  let pBorderWidth :: Ptr FfiStyleBorderWidth = ptrStructStyleBorderWidth ffiStyleAttrs
  pokeStyleBorderWidth (styleBorderWidth attrs) pBorderWidth

  let pBorderColor :: Ptr FfiStyleBorderColor = ptrStructStyleBorderColor ffiStyleAttrs
  pokeStyleBorderColor (styleBorderColor attrs) pBorderColor

  let pMargin :: Ptr FfiStyleMargin = ptrStructStyleMargin ffiStyleAttrs
  pokeStyleMargin (styleMargin attrs) pMargin

  let pPadding :: Ptr FfiStylePadding = ptrStructStylePadding ffiStyleAttrs
  pokeStylePadding (stylePadding attrs) pPadding

  let pTextIndent :: Ptr FfiDwLength = ptrStructStyleTextIndent ffiStyleAttrs
  pokeDwLength (styleTextIndent attrs) pTextIndent

  let pWidth :: Ptr FfiDwLength = ptrStructWidth ffiStyleAttrs
  pokeDwLength (styleWidth attrs) pWidth

  let pHeight :: Ptr FfiDwLength = ptrStructHeight ffiStyleAttrs
  pokeDwLength (styleHeight attrs) pHeight

  let pLineHeight :: Ptr FfiDwLength = ptrStructLineHeight ffiStyleAttrs
  pokeDwLength (styleLineHeight attrs) pLineHeight

  let cTextAlign      :: CInt = fromIntegral . styleTextAlign $ attrs
  let cTextDecoration :: CInt = fromIntegral . styleTextDecoration $ attrs
  let cTextTransform  :: CInt = fromIntegral . styleTextTransform $ attrs

  let cVerticalAlign  :: CInt = fromIntegral . styleVerticalAlign $ attrs
  let cWhiteSpace     :: CInt = fromIntegral . styleWhiteSpace $ attrs

  let cListStylePosition    :: CInt = fromIntegral . styleListStylePosition $ attrs
  let cListStyleType        :: CInt = fromIntegral . styleListStyleType $ attrs

  let cDisplay        :: CInt = fromIntegral . styleDisplay $ attrs
  let cColor          :: CInt = fromIntegral . styleColor $ attrs
  let cCursor         :: CInt = fromIntegral . styleCursor $ attrs
  let cHBorderSpacing :: CInt = fromIntegral . styleHBorderSpacing $ attrs
  let cVBorderSpacing :: CInt = fromIntegral . styleVBorderSpacing $ attrs

  poke ptrStructStyleAttrs $ FfiStyleAttrs pBorderStyle pBorderWidth pBorderColor pMargin pPadding cTextAlign cTextDecoration pTextIndent cTextTransform cVerticalAlign cWhiteSpace pWidth pHeight pLineHeight cListStylePosition cListStyleType cDisplay cColor cCursor cHBorderSpacing cVBorderSpacing




