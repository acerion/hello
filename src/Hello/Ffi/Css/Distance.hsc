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




module Hello.Ffi.Css.Distance
  (
    FfiCssLength (..)

  , cssLengthTypeNone
  , cssLengthTypePX
  , cssLengthTypeMM
  , cssLengthTypeEM
  , cssLengthTypeEX
  , cssLengthTypePercentage
  , cssLengthTypeRelative
  , cssLengthTypeAuto
  )
where




import Prelude
import Foreign
import Foreign.C.Types
--import Debug.Trace




#include "../../hello.h"




data CssLength = CssLength Float Int -- value + type
  deriving (Show, Eq)




data FfiCssLength = FfiCssLength
  { lenValueC :: Float
  , lenTypeC  :: CInt
  } deriving (Show)




instance Storable FfiCssLength where
  sizeOf    _ = #{size c_css_length_t}
  alignment _ = #{alignment c_css_length_t}

  poke ptr (FfiCssLength argValue argType) = do
    #{poke c_css_length_t, c_length_value} ptr argValue
    #{poke c_css_length_t, c_length_type}  ptr argType

  peek ptr = do
    a <- #{peek c_css_length_t, c_length_value} ptr
    b <- #{peek c_css_length_t, c_length_type}  ptr
    return (FfiCssLength a b)




cssLengthTypeNone :: Int
cssLengthTypeNone       = 0
cssLengthTypePX :: Int
cssLengthTypePX         = 1
cssLengthTypeMM :: Int
cssLengthTypeMM         = 2
cssLengthTypeEM :: Int
cssLengthTypeEM         = 3
cssLengthTypeEX :: Int
cssLengthTypeEX         = 4
cssLengthTypePercentage :: Int
cssLengthTypePercentage = 5
cssLengthTypeRelative :: Int
cssLengthTypeRelative   = 6
cssLengthTypeAuto :: Int
cssLengthTypeAuto       = 7




