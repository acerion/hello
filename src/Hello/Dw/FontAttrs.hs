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
Copyright assignments from the file:
 * Copyright 2005-2007 Sebastian Geerken <sgeerken@dillo.org>
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Dw.FontAttrs
  (
    FontAttrs (..)
  , defaultFontAttrs
  )
where




import Prelude
import qualified Data.Text as T
--import Debug.Trace




data FontAttrs = FontAttrs
  {
    fontSize          :: Int
  , fontWeight        :: Int
  , fontName          :: T.Text
  , fontVariant       :: Int
  , fontStyle         :: Int

  , fontXHeight       :: Int
  , fontLetterSpacing :: Int
  } deriving (Show, Eq)




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




