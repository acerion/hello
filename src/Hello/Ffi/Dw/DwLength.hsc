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
foreign export ccall "ffiCreateAutoDwLength" ffiCreateAutoDwLength :: Ptr FfiDwLength -> IO ()

foreign export ccall "ffiIsAutoDwLength" ffiIsAutoDwLength :: Ptr FfiDwLength -> IO Bool
foreign export ccall "ffiIsAbsoluteDwLength" ffiIsAbsoluteDwLength :: Ptr FfiDwLength -> IO Bool
foreign export ccall "ffiIsPercentageDwLength" ffiIsPercentageDwLength :: Ptr FfiDwLength -> IO Bool

foreign export ccall "ffiGetAbsoluteDwLengthValue" ffiGetAbsoluteDwLengthValue :: Ptr FfiDwLength -> IO CInt
foreign export ccall "ffiGetPercentageDwLengthValue" ffiGetPercentageDwLengthValue :: Ptr FfiDwLength -> IO CDouble

foreign export ccall "ffiGetDwLengthHash" ffiGetDwLengthHash :: Ptr FfiDwLength -> IO CInt


#include "../../hello.h"




data FfiDwLength = FfiDwLength
  {
    cDwLengthValue :: CDouble
  , cDwLengthType  :: CInt
  } deriving (Show)




instance Storable FfiDwLength where
  sizeOf    _ = #{size DwLength}
  alignment _ = #{alignment DwLength}

  poke ptr (FfiDwLength v t) = do
    #{poke DwLength, dw_length_value} ptr v
    #{poke DwLength, dw_length_type}  ptr t
    -- #{poke DwLength, dw_length_hash}  ptr h

  peek ptr = do
    v <- #{peek DwLength, dw_length_value} ptr
    t <- #{peek DwLength, dw_length_type}  ptr
    -- h <- #{peek DwLength, dw_length_hash}  ptr
    return (FfiDwLength v t)




peekDwLength :: Ptr FfiDwLength -> IO DwLength
peekDwLength ptrStructDwLength = do
  ffiLength <- peek ptrStructDwLength
  let dwLengthType = fromIntegral . cDwLengthType $ ffiLength
  let len | dwLengthType == dwTypePerc = DwLengthPercentage (coerce . cDwLengthValue $ ffiLength)
          | dwLengthType == dwTypeAbs  = DwLengthAbsolute (floor . cDwLengthValue $ ffiLength)
          | otherwise                  = DwLengthAuto
  return len




pokeDwLength :: DwLength -> Ptr FfiDwLength -> IO ()
pokeDwLength len ptrStructDwLength = do
  let ffiLen = case len of
                 DwLengthPercentage v -> FfiDwLength (coerce v)       (fromIntegral dwTypePerc)
                 DwLengthAbsolute v   -> FfiDwLength (fromIntegral v) (fromIntegral dwTypeAbs)
                 _                    -> FfiDwLength 0                (fromIntegral dwTypeAuto)
  poke ptrStructDwLength ffiLen




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




ffiIsAutoDwLength :: Ptr FfiDwLength -> IO Bool
ffiIsAutoDwLength ptrStructDwLength = do
  len <- peekDwLength ptrStructDwLength
  return . isAutoDwLength $ len




ffiIsAbsoluteDwLength :: Ptr FfiDwLength -> IO Bool
ffiIsAbsoluteDwLength ptrStructDwLength = do
  len <- peekDwLength ptrStructDwLength
  return . isAbsoluteDwLength $ len




ffiIsPercentageDwLength :: Ptr FfiDwLength -> IO Bool
ffiIsPercentageDwLength ptrStructDwLength = do
  len <- peekDwLength ptrStructDwLength
  return . isPercentageDwLength $ len




ffiGetAbsoluteDwLengthValue :: Ptr FfiDwLength -> IO CInt
ffiGetAbsoluteDwLengthValue ptrStructDwLength = do
  len <- peekDwLength ptrStructDwLength
  return . fromIntegral $ (case getAbsoluteDwLengthValue len of
                             Just v  -> v
                             Nothing -> 0)




ffiGetPercentageDwLengthValue :: Ptr FfiDwLength -> IO CDouble
ffiGetPercentageDwLengthValue ptrStructDwLength = do
  len <- peekDwLength ptrStructDwLength
  return . coerce $ (case getPercentageDwLengthValue len of
                       Just v  -> v
                       Nothing -> 0.0)




ffiGetDwLengthHash :: Ptr FfiDwLength -> IO CInt
ffiGetDwLengthHash ptrStructDwLength = do
  len <- peekDwLength ptrStructDwLength
  return . fromIntegral . getDwLengthHash $ len

