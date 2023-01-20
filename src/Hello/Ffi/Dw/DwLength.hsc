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




module Hello.Ffi.Dw.DwLength
  (
    FfiDwLength (..)
  , peekDwLength
  , pokeDwLength

  , ffiCreatePercentageDwLength
  , ffiCreateAbsoluteDwLength
  , ffiCreateAutoDwLength
  )
where




import Prelude
import Foreign
import Foreign.C.Types
--import Debug.Trace

-- https://ro-che.info/articles/2019-05-14-convert-cdouble-to-double
-- I bet I'm doing this wrong.
import Data.Coerce (coerce)

import Hello.Dw.DwLength




foreign export ccall "ffiCreatePercentageDwLength" ffiCreatePercentageDwLength :: Ptr FfiDwLength -> CDouble -> IO ()
foreign export ccall "ffiCreateAbsoluteDwLength" ffiCreateAbsoluteDwLength :: Ptr FfiDwLength -> CInt -> IO ()




#include "../../hello.h"




data FfiDwLength = FfiDwLength
  {
    cDwLengthValue :: CDouble
  , cDwLengthType  :: CInt
  , cDwLengthHash  :: CInt
  } deriving (Show)




instance Storable FfiDwLength where
  sizeOf    _ = #{size DwLength}
  alignment _ = #{alignment DwLength}

  poke ptr (FfiDwLength v t h) = do
    #{poke DwLength, dw_length_value} ptr v
    #{poke DwLength, dw_length_type}  ptr t
    #{poke DwLength, dw_length_hash}  ptr h

  peek ptr = do
    v <- #{peek DwLength, dw_length_value} ptr
    t <- #{peek DwLength, dw_length_type}  ptr
    h <- #{peek DwLength, dw_length_hash}  ptr
    return (FfiDwLength v t h)




peekDwLength :: Ptr FfiDwLength -> IO DwLength
peekDwLength ptrStructDwLength = do
  ffiLength <- peek ptrStructDwLength
  return DwLength
    {
      dwLengthValue = coerce . cDwLengthValue $ ffiLength
    , dwLengthType  = fromIntegral . cDwLengthType  $ ffiLength
    , dwLengthHash  = fromIntegral . cDwLengthHash  $ ffiLength
    }




pokeDwLength :: DwLength -> Ptr FfiDwLength -> IO ()
pokeDwLength len ptrStructDwLength = do
  let v = coerce . dwLengthValue $ len
  let t = fromIntegral . dwLengthType  $ len
  let h = fromIntegral . dwLengthHash  $ len

  poke ptrStructDwLength $ FfiDwLength v t h




ffiCreatePercentageDwLength :: Ptr FfiDwLength -> CDouble -> IO ()
ffiCreatePercentageDwLength ptrStructDwLength cValue = do
  let value = coerce cValue
  let len   = createPercentageDwLength value
  pokeDwLength len ptrStructDwLength
  return ()




ffiCreateAbsoluteDwLength :: Ptr FfiDwLength -> CInt -> IO ()
ffiCreateAbsoluteDwLength ptrStructDwLength cValue = do
  let value = fromIntegral cValue
  let len   = createAbsoluteDwLength value
  pokeDwLength len ptrStructDwLength
  return ()




ffiCreateAutoDwLength :: Ptr FfiDwLength -> IO ()
ffiCreateAutoDwLength ptrStructDwLength = do
  let len = createAutoDwLength
  pokeDwLength len ptrStructDwLength
  return ()


