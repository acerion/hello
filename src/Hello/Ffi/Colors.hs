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

This file is derived from dillo-3.0.5/src/colors.c.
Copyright assignments from that file:
Copyright (C) 2000-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE ForeignFunctionInterface #-}




module Hello.Ffi.Colors
  (
  )
where




import Prelude
import Foreign.C.String
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU
import Hello.Colors




foreign export ccall "ffiColorsStringToColor" ffiColorsStringToColor ::  CString -> Int -> IO Int
foreign export ccall "ffiColorsVisitedColor"  ffiColorsVisitedColor :: Int -> Int -> Int -> Int -> IO Int




ffiColorsStringToColor :: CString -> Int -> IO Int
ffiColorsStringToColor cBuf defaultColor = do
  buf <- BSU.unsafePackCString cBuf
  return (colorsStringToColorWithDefault (T.E.decodeUtf8 buf) defaultColor)




ffiColorsVisitedColor :: Int -> Int -> Int -> Int -> IO Int
ffiColorsVisitedColor candidate txt lnk bg = do
  return (colorsVisitedColor candidate txt lnk bg)



