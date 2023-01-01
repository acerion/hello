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




module Hello.Ffi.Preferences
  (
    peekPreferences
  , FfiPreferences
  )
where




import Prelude
import Foreign
import Foreign.C.Types
--import Debug.Trace

import Hello.Ffi.Utils

import Hello.Preferences




#include "../hello.h"




data FfiPreferences = FfiPreferences
  {
    cPrefsFontSerif      :: Ptr CChar
  , cPrefsFontSansSerif  :: Ptr CChar
  , cPrefsFontCursive    :: Ptr CChar
  , cPrefsFontFantasy    :: Ptr CChar
  , cPrefsFontMonospace  :: Ptr CChar
  , cPrefsFontFactor     :: Float
  , cPrefsFontMinSize    :: CInt
  , cPrefsFontMaxSize    :: CInt
  }




instance Storable FfiPreferences where
  sizeOf    _ = #{size c_prefs_t}
  alignment _ = #{alignment c_prefs_t}

  peek ptr = do
    a <- #{peek c_prefs_t, font_serif}            ptr
    b <- #{peek c_prefs_t, font_sans_serif}       ptr
    c <- #{peek c_prefs_t, font_cursive}          ptr
    d <- #{peek c_prefs_t, font_fantasy}          ptr
    e <- #{peek c_prefs_t, font_monospace}        ptr
    f <- #{peek c_prefs_t, font_factor}           ptr
    g <- #{peek c_prefs_t, font_min_size}         ptr
    h <- #{peek c_prefs_t, font_max_size}         ptr
    return (FfiPreferences a b c d e f g h)

  poke ptr (FfiPreferences a b c d e f g h) = do
    #{poke c_prefs_t, font_serif}       ptr a
    #{poke c_prefs_t, font_sans_serif}  ptr b
    #{poke c_prefs_t, font_cursive}     ptr c
    #{poke c_prefs_t, font_fantasy}     ptr d
    #{poke c_prefs_t, font_monospace}   ptr e
    #{poke c_prefs_t, font_factor}      ptr f
    #{poke c_prefs_t, font_min_size}    ptr g
    #{poke c_prefs_t, font_max_size}    ptr h




peekPreferences :: Ptr FfiPreferences -> IO Preferences
peekPreferences ptrStructPreferences = do

  ffiPrefs <- peek ptrStructPreferences

  fontSerif       <- ptrCCharToText . cPrefsFontSerif $ ffiPrefs
  fontSansSerif   <- ptrCCharToText . cPrefsFontSansSerif $ ffiPrefs
  fontCursive     <- ptrCCharToText . cPrefsFontCursive $ ffiPrefs
  fontFantasy     <- ptrCCharToText . cPrefsFontFantasy $ ffiPrefs
  fontMonospace   <- ptrCCharToText . cPrefsFontMonospace $ ffiPrefs
  let fontFactor  = cPrefsFontFactor ffiPrefs
  let fontMinSize = fromIntegral . cPrefsFontMinSize $ ffiPrefs
  let fontMaxSize = fromIntegral . cPrefsFontMaxSize $ ffiPrefs

  return Preferences
    {
      prefsFontSerif     = fontSerif
    , prefsFontSansSerif = fontSansSerif
    , prefsFontCursive   = fontCursive
    , prefsFontFantasy   = fontFantasy
    , prefsFontMonospace = fontMonospace
    , prefsFontFactor    = fontFactor
    , prefsFontMinSize   = fontMinSize
    , prefsFontMaxSize   = fontMaxSize
    }



