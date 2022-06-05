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




module Hello.Ffi.Css.Value
  (
    makeValue
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Debug.Trace

import Hello.Css.ContextGlobal
import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.UserAgentStyle

import Hello.Ffi.Css.Distance
import Hello.Ffi.Utils




makeValue valType intVal textVal lengthVal lengthType | valType ==  0 = CssValueTypeInt intVal
                                                      | valType ==  1 = CssValueTypeEnum intVal
                                                      | valType ==  2 = CssValueTypeMultiEnum intVal
                                                      | valType ==  3 = CssValueTypeLengthPercent       $ cssLengthToDistance lengthVal lengthType
                                                      | valType ==  4 = CssValueTypeLength              $ cssLengthToDistance lengthVal lengthType
                                                      | valType ==  5 = CssValueTypeSignedLength        $ cssLengthToDistance lengthVal lengthType
                                                      | valType ==  6 = CssValueTypeLengthPercentNumber $ cssLengthToDistance lengthVal lengthType
                                                      | valType ==  7 = CssValueTypeAuto                $ cssLengthToDistance lengthVal lengthType
                                                      | valType ==  8 = CssValueTypeColor intVal
                                                      | valType ==  9 = CssValueTypeFontWeight intVal
                                                      | valType == 10 = CssValueTypeString textVal
                                                      | valType == 11 = CssValueTypeStringList textVal
                                                      | valType == 12 = CssValueTypeURI textVal
                                                      | valType == 13 = CssValueTypeBgPosition
                                                      | otherwise = CssValueTypeUnused
