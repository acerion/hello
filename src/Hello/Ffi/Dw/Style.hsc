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
  , ptrStructStyleMargin       :: Ptr FfiStyleMargin
  , ptrStructStylePadding      :: Ptr FfiStylePadding
  , iStyleTextAlign            :: CInt
  , iTextDecoration            :: CInt
  , ptrStructStyleTextIndent   :: Ptr FfiDwLength
  , iTextTransform             :: CInt
  } deriving (Show)




instance Storable FfiStyleAttrs where
  sizeOf    _ = #{size c_style_attrs_t}
  alignment _ = #{alignment c_style_attrs_t}

  peek ptr = do
    borderStyle      <- #{peek c_style_attrs_t, c_border_style}      ptr
    borderWidth      <- #{peek c_style_attrs_t, c_border_width}      ptr
    margin           <- #{peek c_style_attrs_t, c_margin}            ptr
    padding          <- #{peek c_style_attrs_t, c_padding}           ptr
    textAlign        <- #{peek c_style_attrs_t, c_text_align}        ptr
    textDecoration   <- #{peek c_style_attrs_t, c_text_decoration}   ptr
    textIndent       <- #{peek c_style_attrs_t, c_text_indent}       ptr
    textTransform    <- #{peek c_style_attrs_t, c_text_transform}    ptr
    return (FfiStyleAttrs borderStyle borderWidth margin padding textAlign textDecoration textIndent textTransform)


  poke ptr (FfiStyleAttrs cBorderStyle cBorderWidth cMargin cPadding cTextAlign cTextDecoration cTextIndent cTextTransform) = do
    #{poke c_style_attrs_t, c_border_style}    ptr cBorderStyle
    #{poke c_style_attrs_t, c_border_width}    ptr cBorderWidth
    #{poke c_style_attrs_t, c_margin}          ptr cMargin
    #{poke c_style_attrs_t, c_padding}         ptr cPadding
    #{poke c_style_attrs_t, c_text_align}      ptr cTextAlign
    #{poke c_style_attrs_t, c_text_decoration} ptr cTextDecoration
    #{poke c_style_attrs_t, c_text_indent}     ptr cTextIndent
    #{poke c_style_attrs_t, c_text_transform}  ptr cTextTransform




peekStyleAttrs :: Ptr FfiStyleAttrs -> IO StyleAttrs
peekStyleAttrs ptrStructStyleAttrs = do
  ffiAttrs <- peek ptrStructStyleAttrs

  borderStyle <- peekStyleBorderStyle . ptrStructStyleBorderStyle $ ffiAttrs
  borderWidth <- peekStyleBorderWidth . ptrStructStyleBorderWidth $ ffiAttrs
  margin      <- peekStyleMargin . ptrStructStyleMargin $ ffiAttrs
  padding     <- peekStylePadding . ptrStructStylePadding $ ffiAttrs
  tIndent     <- peekDwLength . ptrStructStyleTextIndent $ ffiAttrs

  return StyleAttrs
    {
      styleBorderStyle = borderStyle
    , styleBorderWidth = borderWidth
    , styleMargin      = margin
    , stylePadding     = padding

    , styleTextAlign      = fromIntegral . iStyleTextAlign $ ffiAttrs
    , styleTextDecoration = fromIntegral . iTextDecoration $ ffiAttrs
    , styleTextIndent     = tIndent
    , styleTextTransform  = fromIntegral . iTextTransform $ ffiAttrs
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

  let pMargin :: Ptr FfiStyleMargin = ptrStructStyleMargin ffiStyleAttrs
  pokeStyleMargin (styleMargin attrs) pMargin

  let pPadding :: Ptr FfiStylePadding = ptrStructStylePadding ffiStyleAttrs
  pokeStylePadding (stylePadding attrs) pPadding

  let pTextIndent :: Ptr FfiDwLength = ptrStructStyleTextIndent ffiStyleAttrs
  pokeDwLength (styleTextIndent attrs) pTextIndent

  let cTextAlign      :: CInt = fromIntegral . styleTextAlign $ attrs
  let cTextDecoration :: CInt = fromIntegral . styleTextDecoration $ attrs
  let cTextTransform  :: CInt = fromIntegral . styleTextTransform $ attrs

  poke ptrStructStyleAttrs $ FfiStyleAttrs pBorderStyle pBorderWidth pMargin pPadding cTextAlign cTextDecoration pTextIndent cTextTransform




