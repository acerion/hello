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
Copyright assignments from the file:
 * Copyright 2005-2007 Sebastian Geerken <sgeerken@dillo.org>
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




{-
https://drafts.csswg.org/css-fonts-3/#propdef-font-style
-}




module Hello.Dw.FontAttrs
  (
    FontAttrs (..)
  , defaultFontAttrs
  , defaultFontAttrsFromPreferences
  )
where




import Prelude
import qualified Data.Text as T
--import Debug.Trace

import Hello.Preferences
import Hello.Utils




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




defaultFontAttrs :: FontAttrs
defaultFontAttrs = FontAttrs
  {
    fontSize    = 14   -- In other places of dillo/hello this value appears (multiplied by font factor)
  , fontWeight  = 400  -- A value corresponding to NORMAL.
  , fontName    = ""
  , fontVariant = 0  -- FONT_VARIANT_NORMAL == 0
  , fontStyle   = 0  -- FONT_STYLE_NORMAL == 0

  , fontXHeight       = 6     -- Value most often appearing in debugs.
  , fontLetterSpacing = 0
  }




defaultFontAttrsFromPreferences :: Preferences -> FontAttrs
defaultFontAttrsFromPreferences prefs = defaultFontAttrs
  { fontSize = clipSize . roundInt $ (14 * (prefsFontFactor prefs))
  , fontName = prefsFontSansSerif prefs
  }

  where
    clipSize size | size < prefsFontMinSize prefs = prefsFontMinSize prefs
                  | size > prefsFontMaxSize prefs = prefsFontMaxSize prefs
                  | otherwise                     = size

