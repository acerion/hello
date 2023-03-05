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
import Foreign.C.Types
--import Debug.Trace

import Hello.Dw.Style
import Hello.Dw.StyleAttrsGlobal

import Hello.Ffi.Dw.DwLength
import Hello.Ffi.Dw.FontAttrs
import Hello.Ffi.Utils




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




data FfiStyleAttrs = FfiStyleAttrs
  {
    iStyleAttrsRef             :: CInt
  , ptrStructFontAttrs         :: Ptr FfiFontAttrs
  , ptrStructStyleBorderWidth  :: Ptr FfiStyleBorderWidth
  , ptrStructStyleBorderColor  :: Ptr FfiStyleBorderColor
  , ptrStructStyleMargin       :: Ptr FfiStyleMargin
  , ptrStructStylePadding      :: Ptr FfiStylePadding
  , ptrStructStyleTextIndent   :: Ptr FfiDwLength
  , iVerticalAlign             :: CInt
  , ptrStructLineHeight        :: Ptr FfiDwLength
  , iDisplay                   :: CInt
  , iColor                     :: CInt
  , iBackgroundColor           :: CInt
  , iHBorderSpacing            :: CInt
  , iVBorderSpacing            :: CInt
  , iWordSpacing               :: CInt
  , ptrCharXLang               :: Ptr CChar -- buffer of specified size.
  , ptrCharXTooltip            :: Ptr CChar -- pointer to to-be-allocated memory
  } deriving (Show)




instance Storable FfiStyleAttrs where
  sizeOf    _ = #{size c_style_attrs_t}
  alignment _ = #{alignment c_style_attrs_t}

  peek ptr = do
    ref              <- #{peek c_style_attrs_t, c_style_attrs_ref}   ptr
    fontAttrs        <- #{peek c_style_attrs_t, c_font_attrs}        ptr
    borderWidth      <- #{peek c_style_attrs_t, c_border_width}      ptr
    borderColor      <- #{peek c_style_attrs_t, c_border_color}      ptr
    margin           <- #{peek c_style_attrs_t, c_margin}            ptr
    padding          <- #{peek c_style_attrs_t, c_padding}           ptr
    textIndent       <- #{peek c_style_attrs_t, c_text_indent}       ptr
    verticalAlign    <- #{peek c_style_attrs_t, c_vertical_align}    ptr
    lineHeight       <- #{peek c_style_attrs_t, c_line_height}       ptr
    display            <- #{peek c_style_attrs_t, c_display}              ptr
    color              <- #{peek c_style_attrs_t, c_color}                ptr
    backgroundColor    <- #{peek c_style_attrs_t, c_background_color}     ptr
    hBorderSpacing     <- #{peek c_style_attrs_t, c_h_border_spacing}     ptr
    vBorderSpacing     <- #{peek c_style_attrs_t, c_v_border_spacing}     ptr
    wordSpacing        <- #{peek c_style_attrs_t, c_word_spacing}         ptr
    let xLang = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_style_attrs_t, c_x_lang}) ptr
    xTooltip           <- #{peek c_style_attrs_t, c_x_tooltip}            ptr

    return (FfiStyleAttrs ref fontAttrs borderWidth borderColor margin padding textIndent verticalAlign lineHeight display color backgroundColor hBorderSpacing vBorderSpacing wordSpacing xLang xTooltip)




  poke ptr (FfiStyleAttrs cStyleAttrsRef cFontAttrs cBorderWidth cBorderColor cMargin cPadding cTextIndent cVerticalAlign cLineHeight cDisplay cColor cBackgroundColor cHBorderSpacing cVBorderSpacing cWordSpacing _cXLang cXTooltip) = do

    #{poke c_style_attrs_t, c_style_attrs_ref} ptr cStyleAttrsRef
    #{poke c_style_attrs_t, c_font_attrs}      ptr cFontAttrs
    #{poke c_style_attrs_t, c_border_width}    ptr cBorderWidth
    #{poke c_style_attrs_t, c_border_color}    ptr cBorderColor
    #{poke c_style_attrs_t, c_margin}          ptr cMargin
    #{poke c_style_attrs_t, c_padding}         ptr cPadding
    #{poke c_style_attrs_t, c_text_indent}     ptr cTextIndent
    #{poke c_style_attrs_t, c_vertical_align}  ptr cVerticalAlign
    #{poke c_style_attrs_t, c_line_height}          ptr cLineHeight
    #{poke c_style_attrs_t, c_display}              ptr cDisplay
    #{poke c_style_attrs_t, c_color}                ptr cColor
    #{poke c_style_attrs_t, c_background_color}     ptr cBackgroundColor
    #{poke c_style_attrs_t, c_h_border_spacing}     ptr cHBorderSpacing
    #{poke c_style_attrs_t, c_v_border_spacing}     ptr cVBorderSpacing
    #{poke c_style_attrs_t, c_word_spacing}         ptr cWordSpacing
    -- #{poke c_style_attrs_t, c_x_lang}               ptr cXLang -- Poking of this field is done in pokeStyleAttrs
    #{poke c_style_attrs_t, c_x_tooltip}            ptr cXTooltip




peekStyleAttrs :: Ptr FfiStyleAttrs -> IO StyleAttrs
peekStyleAttrs ptrStructStyleAttrs = do
  ffiAttrs <- peek ptrStructStyleAttrs

  fontAttrs   <- peekFontAttrs . ptrStructFontAttrs $ ffiAttrs
  borderWidth <- peekStyleBorderWidth . ptrStructStyleBorderWidth $ ffiAttrs
  borderColor <- peekStyleBorderColor . ptrStructStyleBorderColor $ ffiAttrs
  margin      <- peekStyleMargin . ptrStructStyleMargin $ ffiAttrs
  padding     <- peekStylePadding . ptrStructStylePadding $ ffiAttrs
  tIndent     <- peekDwLength . ptrStructStyleTextIndent $ ffiAttrs

  lineHeight <- peekDwLength . ptrStructLineHeight $ ffiAttrs

  xLang  <- peekCharBuffer (ptrCharXLang ffiAttrs)
  xTooltip  <- peekCharBuffer (ptrCharXTooltip ffiAttrs)

  gAttrs <- globalStyleAttrsGet . fromIntegral . iStyleAttrsRef $ ffiAttrs

  let attrs = defaultStyleAttrs {
      styleAttrsRef    = styleAttrsRef gAttrs
    , styleFontAttrs   = fontAttrs
    , styleBorderCollapse = styleBorderCollapse gAttrs
    , styleBorderStyle = styleBorderStyle gAttrs
    , styleBorderWidth = borderWidth
    , styleBorderColor = borderColor
    , styleMargin      = margin
    , stylePadding     = padding

    , styleTextAlign      = styleTextAlign gAttrs
    , styleTextDecoration = styleTextDecoration gAttrs
    , styleTextIndent     = tIndent
    , styleTextTransform  = styleTextTransform gAttrs

    , styleVerticalAlign  = fromIntegral . iVerticalAlign $ ffiAttrs
    , styleWhiteSpace     = styleWhiteSpace gAttrs

    , styleWidth          = styleWidth gAttrs
    , styleHeight         = styleHeight gAttrs
    , styleLineHeight     = lineHeight

    , styleListStylePosition      = styleListStylePosition gAttrs
    , styleListStyleType          = styleListStyleType gAttrs

    , styleDisplay                = fromIntegral . iDisplay $ ffiAttrs
    , styleColor                  = fromIntegral . iColor $ ffiAttrs
    , styleBackgroundColor        = fromIntegral . iBackgroundColor $ ffiAttrs
    , styleCursor                 = styleCursor gAttrs
    , styleHBorderSpacing         = fromIntegral . iHBorderSpacing $ ffiAttrs
    , styleVBorderSpacing         = fromIntegral . iVBorderSpacing $ ffiAttrs
    , styleWordSpacing            = fromIntegral . iWordSpacing $ ffiAttrs
    , styleXLink                  = styleXLink gAttrs
    , styleXLang                  = xLang
    , styleXImg                   = styleXImg gAttrs
    , styleXTooltip               = xTooltip
    }

  return attrs




pokeStyleAttrs :: StyleAttrs -> Ptr FfiStyleAttrs -> IO ()
pokeStyleAttrs attrs ptrStructStyleAttrs = do

  -- I'm peeking a pointer in 'pokeStyleAttrs' function to get access to
  -- pointer-members of ptrStructStyleAttrs. When I will have access to the
  -- pointer-members, I will be able to poke them with values passed through
  -- 'attrs'.
  ffiStyleAttrs :: FfiStyleAttrs <- peek ptrStructStyleAttrs

  -- getAccess to member-pointers, and then poke them
  let pFontAttrs :: Ptr FfiFontAttrs = ptrStructFontAttrs ffiStyleAttrs
  pokeFontAttrs (styleFontAttrs attrs) pFontAttrs

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

  let pLineHeight :: Ptr FfiDwLength = ptrStructLineHeight ffiStyleAttrs
  pokeDwLength (styleLineHeight attrs) pLineHeight

  let cStyleAttrsRef :: CInt = fromIntegral . styleAttrsRef $ attrs

  let cVerticalAlign  :: CInt = fromIntegral . styleVerticalAlign $ attrs

  let cDisplay        :: CInt = fromIntegral . styleDisplay $ attrs
  let cColor          :: CInt = fromIntegral . styleColor $ attrs
  let cBackgroundColor :: CInt = fromIntegral . styleBackgroundColor $ attrs
  let cHBorderSpacing :: CInt = fromIntegral . styleHBorderSpacing $ attrs
  let cVBorderSpacing :: CInt = fromIntegral . styleVBorderSpacing $ attrs
  let cWordSpacing    :: CInt = fromIntegral . styleWordSpacing $ attrs

  let bufXLang :: Ptr CChar = ptrCharXLang ffiStyleAttrs
  -- "((c_style_attrs_t *)0)->c_x_lang" is a C trick that happens to work with hsc2hs.
  pokeCharBuffer bufXLang #{size ((c_style_attrs_t *)0)->c_x_lang} (styleXLang attrs)
  -- Dummy arg. 'poke' function won't be poking a field in the struct - it
  -- has been already done in two lines above.
  let cXLang = nullPtr

  cXTooltip :: Ptr CChar <- allocAndPokeCString . styleXTooltip $ attrs -- TODO: this allocates memory that is not freed anywhere

  poke ptrStructStyleAttrs $ FfiStyleAttrs cStyleAttrsRef pFontAttrs pBorderWidth pBorderColor pMargin pPadding pTextIndent cVerticalAlign pLineHeight cDisplay cColor cBackgroundColor cHBorderSpacing cVBorderSpacing cWordSpacing cXLang cXTooltip




