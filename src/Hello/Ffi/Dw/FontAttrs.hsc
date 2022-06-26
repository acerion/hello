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




module Hello.Ffi.Dw.FontAttrs
  (
    FfiFontAttrs (..)
  , peekFontAttrs
  , pokeFontAttrs
  )
where




import Prelude
import Debug.Trace
import qualified Data.Text as T

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Hello.Dw.FontAttrs

import Hello.Ffi.Utils




#include "../../hello.h"




data FfiFontAttrs = FfiFontAttrs
  {
    cFontSize          :: CInt
  , cFontWeight        :: CInt
  , cFontName          :: Ptr CChar
  , cFontVariant       :: CInt
  , cFontStyle         :: CInt

  , cFontXHeight       :: CInt
  , cFontLetterSpacing :: CInt
  } deriving (Show)




instance Storable FfiFontAttrs where
  sizeOf    _ = #{size c_font_attrs_t}
  alignment _ = #{alignment c_font_attrs_t}

  poke ptr (FfiFontAttrs argSize argWeight argName argFontVariant argStyle argXHeight argLetterSpacing) = do
    #{poke c_font_attrs_t, size}          ptr argSize
    #{poke c_font_attrs_t, weight}        ptr argWeight
    #{poke c_font_attrs_t, name}          ptr argName
    #{poke c_font_attrs_t, fontVariant}   ptr argFontVariant
    #{poke c_font_attrs_t, style}         ptr argStyle
    #{poke c_font_attrs_t, xHeight}       ptr argXHeight
    #{poke c_font_attrs_t, letterSpacing} ptr argLetterSpacing

  peek ptr = do
    a <- #{peek c_font_attrs_t, size} ptr
    b <- #{peek c_font_attrs_t, weight}  ptr
    c <- #{peek c_font_attrs_t, name} ptr
    d <- #{peek c_font_attrs_t, fontVariant} ptr
    e <- #{peek c_font_attrs_t, style} ptr
    f <- #{peek c_font_attrs_t, xHeight} ptr
    g <- #{peek c_font_attrs_t, letterSpacing} ptr
    return (FfiFontAttrs a b c d e f g)




peekFontAttrs :: Ptr FfiFontAttrs -> IO FontAttrs
peekFontAttrs ptrStructFontAttrs = do
  ffiAttrs <- peek ptrStructFontAttrs
  name     <- ptrCCharToText . cFontName $ ffiAttrs

  let fontAttrs = FontAttrs
        {
          fontSize          = fromIntegral . cFontSize $ ffiAttrs
        , fontWeight        = fromIntegral . cFontWeight $ ffiAttrs
        , fontName          = name
        , fontVariant       = fromIntegral . cFontVariant $ ffiAttrs
        , fontStyle         = fromIntegral . cFontStyle $ ffiAttrs
        , fontXHeight       = fromIntegral . cFontXHeight $ ffiAttrs
        , fontLetterSpacing = fromIntegral . cFontLetterSpacing $ ffiAttrs
        }

  return fontAttrs




pokeFontAttrs :: FontAttrs -> Ptr FfiFontAttrs -> IO ()
pokeFontAttrs fontAttrs ptrStructFontAttrs = do
  let size   = fromIntegral . fontSize $ fontAttrs
  let weight = fromIntegral . fontWeight $ fontAttrs
  name <- newCString . T.unpack . fontName $ fontAttrs
  let variant = fromIntegral . fontVariant $ fontAttrs
  let style   = fromIntegral . fontStyle $ fontAttrs
  let height  = fromIntegral . fontXHeight $ fontAttrs
  let spacing = fromIntegral . fontLetterSpacing $ fontAttrs

  poke ptrStructFontAttrs $ FfiFontAttrs size weight name variant style height spacing




