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




{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Ffi.Dw.StyleAttrsGlobal
  (
  )
where




import Prelude
import Foreign.C.Types

--import Debug.Trace
import Data.Word

import Hello.Dw.Style
import Hello.Dw.StyleAttrsGlobal




#include "../../hello.h"




foreign export ccall "ffiStyleAttrsCtor" ffiStyleAttrsCtor :: IO CInt


foreign export ccall "ffiStyleAttrsInitValues" ffiStyleAttrsInitValues :: CInt -> IO ()
foreign export ccall "ffiStyleAttrsEqual" ffiStyleAttrsEqual :: CInt -> CInt -> IO Bool
foreign export ccall "ffiStyleAttrsHashValue" ffiStyleAttrsHashValue :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsCopy" ffiStyleAttrsCopy :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsReset" ffiStyleAttrsReset :: CInt -> IO ()

foreign export ccall "ffiStyleAttrsTextAlign" ffiStyleAttrsTextAlign :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsTextDecoration" ffiStyleAttrsTextDecoration :: CInt -> IO Word32
foreign export ccall "ffiStyleAttrsSetTextDecoration" ffiStyleAttrsSetTextDecoration :: CInt -> Word32 -> IO ()

foreign export ccall "ffiStyleAttrsTextTransform" ffiStyleAttrsTextTransform :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsCursor" ffiStyleAttrsCursor :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetCursor" ffiStyleAttrsSetCursor :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsWhiteSpace" ffiStyleAttrsWhiteSpace :: CInt -> IO CInt




ffiStyleAttrsCtor :: IO CInt
ffiStyleAttrsCtor = do
  ref <- fmap fromIntegral globalStyleAttrsCtor
  return . fromIntegral $ ref




ffiStyleAttrsInitValues :: CInt -> IO ()
ffiStyleAttrsInitValues cRef = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let new = styleAttrsInitValues old
  globalStyleAttrsUpdate ref new
  return ()




ffiStyleAttrsEqual :: CInt -> CInt -> IO Bool
ffiStyleAttrsEqual cRef1 cRef2 = do
  let ref1 = fromIntegral cRef1
  let ref2 = fromIntegral cRef2
  attrs1 <- globalStyleAttrsGet ref1
  attrs2 <- globalStyleAttrsGet ref2
  return $ styleAttrsEqual attrs1 attrs2




ffiStyleAttrsHashValue :: CInt -> IO CInt
ffiStyleAttrsHashValue cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleAttrsHashValue $ attrs




ffiStyleAttrsCopy :: CInt -> CInt -> IO ()
ffiStyleAttrsCopy cRefTo cRefFrom = do
  let refTo   = fromIntegral cRefTo
  let refFrom = fromIntegral cRefFrom
  attrsTo   <- globalStyleAttrsGet refTo
  attrsFrom <- globalStyleAttrsGet refFrom
  let attrs = styleAttrsCopy attrsTo attrsFrom
  globalStyleAttrsUpdate refTo attrs
  return ()




ffiStyleAttrsTextAlign :: CInt -> IO CInt
ffiStyleAttrsTextAlign cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleTextAlign $ attrs




ffiStyleAttrsTextDecoration :: CInt -> IO Word32
ffiStyleAttrsTextDecoration cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleTextDecoration $ attrs




ffiStyleAttrsSetTextDecoration :: CInt -> Word32 -> IO ()
ffiStyleAttrsSetTextDecoration cRef val = do
  let ref = fromIntegral cRef
  --let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let new = old { styleTextDecoration = val }
  globalStyleAttrsUpdate ref new
  return ()




ffiStyleAttrsTextTransform :: CInt -> IO CInt
ffiStyleAttrsTextTransform cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleTextTransform $ attrs




ffiStyleAttrsCursor :: CInt -> IO CInt
ffiStyleAttrsCursor cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleCursor $ attrs




ffiStyleAttrsSetCursor :: CInt -> CInt -> IO ()
ffiStyleAttrsSetCursor cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let new = old { styleCursor = val }
  globalStyleAttrsUpdate ref new
  return ()





ffiStyleAttrsWhiteSpace :: CInt -> IO CInt
ffiStyleAttrsWhiteSpace cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleWhiteSpace $ attrs




ffiStyleAttrsReset :: CInt -> IO ()
ffiStyleAttrsReset cRef = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let new = styleAttrsReset old
  globalStyleAttrsUpdate ref new
  return ()

